package amf.plugins.domain.webapi.resolution.stages

import amf.core.annotations.DefaultNode
import amf.core.metamodel.domain.{DataNodeModel, DomainElementModel, LinkableElementModel}
import amf.core.metamodel.domain.DomainElementModel._
import amf.core.metamodel.domain.extensions.PropertyShapeModel
import amf.core.metamodel.domain.templates.{KeyField, OptionalField}
import amf.core.metamodel.{Field, Type}
import amf.core.model.domain.DataNodeOps.adoptTree
import amf.core.model.domain._
import amf.core.parser.{ErrorHandler, FieldEntry, Value}
import amf.plugins.document.webapi.annotations.Inferred
import amf.plugins.domain.shapes.metamodel.ArrayShapeModel
import amf.plugins.domain.shapes.models.{AnyShape, ArrayShape, NodeShape}
import amf.plugins.features.validation.ParserSideValidations

/**
  * Merge 'other' element into 'main' element:
  * 1) 'main' node properties are inspected and those that are undefined in 'other' node remain unchanged.
  * 2) 'main' node receives all properties of 'other' node (excluding optional ones), which are undefined in the 'main' node.
  * 3) Properties defined in both 'main' node and 'other' node (including optional ones) are treated as follows:
  *   a) Scalar properties remain unchanged.
  *   b) Collection properties are merged by value.
  *   c) Values of object properties are subjected to steps 1-3 of this procedure.
  */
object DomainElementMerging {

  def merge[T <: DomainElement](main: T, other: T, errorHandler: ErrorHandler): T = {
    var merged = false
    other.fields.fields().filter(ignored).foreach {
      case entry @ FieldEntry(field, value) =>
        main.fields.entry(field) match {
          case None => // Case (2)
            field.`type` match {
              case t: OptionalField if isOptional(t, value.value.asInstanceOf[DomainElement]) => // Do nothing (2)
              case Type.ArrayLike(element) => {
                setNonOptional(main, field, element, value)
              }
              case _ => {
                main.set(field, adoptInner(main.id, value.value))
              }
            }

          // special case when we have a default Any shape generated by default being overwritten by a different shape
          // coming from an overlay/extension, for example as the default value of a payload
          case Some(existing)
              if Option(existing.value).isDefined && Option(existing.value.value).isDefined && value.value
                .isInstanceOf[AnyShape]
                && existing.value.value.isInstanceOf[AnyShape] && existing.value.value.annotations.contains(
                classOf[Inferred]) =>
            val target = existing.value.value.asInstanceOf[AnyShape]
            val cloned = value.value.asInstanceOf[AnyShape].cloneShape(None).withName(target.name.value())
            if (target.examples.nonEmpty) cloned.withExamples(target.examples)
            main.set(field, adoptInner(main.id, cloned))

          case Some(existing)
              if Option(existing.value).isDefined && Option(existing.value.value).isDefined
                && existing.value.value.annotations.contains(classOf[DefaultNode]) =>
            field.`type` match {
              case t: OptionalField if isOptional(t, value.value.asInstanceOf[DomainElement]) => // Do nothing (2)
              case Type.ArrayLike(element)                                                    => setNonOptional(main, field, element, value)
              case _                                                                          => main.set(field, adoptInner(main.id, value.value))
            }

          case Some(existing) => // Case (3)
            merged = true
            field.`type` match {
              case _: Type.Scalar          => // Do nothing (3.a)
              case Type.ArrayLike(element) => mergeByValue(main, field, element, existing.value, value, errorHandler)
              case _: DomainElementModel   => merge(existing.domainElement, entry.domainElement, errorHandler)
              case _ =>
                errorHandler.violation(ParserSideValidations.ResolutionErrorSpecification.id,
                                       main.id,
                                       s"Cannot merge '${field.`type`}':not a (Scalar|Array|Object)",
                                       main.annotations)
            }
        }
    }

    main match {
      case shape: Shape if merged => ensureNotRecursive(shape).asInstanceOf[T]
      case _                      => main
    }
  }

  protected def ensureNotRecursive(shape: Shape, ids: Set[String] = Set()): Shape = {
    try {
      if (ids.contains(shape.id))
        shape match {
          case _: RecursiveShape => shape
          case _                 => RecursiveShape(shape)
        } else {
        val newIds = ids ++ Seq(shape.id)
        shape.fields.foreach {
          case (f: Field, value: Value) =>
            val fieldValue  = value.value
            val annotations = value.annotations
            fieldValue match {
              case e: Shape =>
                shape.fields.setWithoutId(f, ensureNotRecursive(e, newIds), annotations)
              case arr: AmfArray =>
                val checked = arr.values.map {
                  case e: Shape => ensureNotRecursive(e, newIds)
                  case o        => o
                }
                shape.fields.setWithoutId(f, AmfArray(checked, arr.annotations))
              case o =>
                shape.fields.setWithoutId(f, o, annotations)
            }
        }
        shape
      }
    } catch {
      case _: Error => shape
    }
  }

  protected case class Adopted() {
    private var adopted: Seq[String] = Nil

    def +=(id: String): Adopted = {
      adopted = adopted :+ id
      this
    }
    def notYet(id: String): Boolean = !adopted.contains(id)
  }

  def adoptInner(id: String, target: AmfElement, adopted: Adopted = Adopted()): AmfElement = {
    target match {
      case array: AmfArray =>
        AmfArray(array.values.map(adoptInner(id, _, adopted)), array.annotations)
      case element: DomainElement if adopted notYet element.id =>
        adoptElementByType(element, id)
        adopted += element.id
        element.fields.foreach {
          case (f, value) => {
            if (ignored(FieldEntry(f, value))) {
              adoptInner(element.id, value.value, adopted)
            }
          }
        }

        element
      case _ => target
    }
  }

  private def adoptElementByType(element: DomainElement, parent: String) = {
    element match {
      case simple: Shape     => simple.simpleAdoption(parent) // only shapes have recursive simple adoption?
      case dynamic: DataNode => DataNodeOps.adoptTree(parent, dynamic)
      case _                 => element.adopted(parent)
    }
  }

  private def setNonOptional(target: DomainElement, field: Field, element: Type, other: Value): Unit = {
    element match {
      case t: OptionalField =>
        val nonOptional =
          other.value.asInstanceOf[AmfArray].values.filter(v => !isOptional(t, v.asInstanceOf[DomainElement]))
        target.set(field, adoptInner(target.id, AmfArray(nonOptional)))
      case _ => target.set(field, adoptInner(target.id, other.value))
    }
  }

  private def mergeByValue(target: DomainElement,
                           field: Field,
                           element: Type,
                           main: Value,
                           other: Value,
                           errorHandler: ErrorHandler): Unit = {
    val m = main.value.asInstanceOf[AmfArray]
    val o = other.value.asInstanceOf[AmfArray]

    element match {
      case _: Type.Scalar => mergeByValue(target, field, m, o)
      case key: KeyField  => mergeByKeyValue(target, field, element, key, m, o, errorHandler)
      case DataNodeModel  => mergeDataNodes(target, field, m, o)
      case _ =>
        errorHandler.violation(ParserSideValidations.ResolutionErrorSpecification.id,
                               target.id,
                               s"Cannot merge '$element': not a KeyField nor a Scalar",
                               target.annotations)

    }
  }

  private def mergeDataNodes(target: DomainElement, field: Field, main: AmfArray, other: AmfArray): Unit = {

    val mainNodes  = main.values.asInstanceOf[Seq[DataNode]]
    val otherNodes = other.values.asInstanceOf[Seq[DataNode]]

    otherNodes.foreach {
      case oScalar: ScalarNode =>
        if (mainNodes.collectFirst({ case ms: ScalarNode if ms.value.equals(oScalar.value) => ms }).isEmpty)
          target.add(field, oScalar)
      case other: DataNode => target.add(field, other)
    }
  }

  private def mergeByValue(target: DomainElement, field: Field, main: AmfArray, other: AmfArray): Unit = {
    val existing = main.values.map(_.asInstanceOf[AmfScalar].value).toSet
    other.values.foreach { value =>
      val scalar = value.asInstanceOf[AmfScalar].value
      if (!existing.contains(scalar)) {
        target.add(field, AmfScalar(scalar)) // Remove annotations so it is added last in the list.
      }
    }
  }

  private def mergeByKeyValue(target: DomainElement,
                              field: Field,
                              element: Type,
                              key: KeyField,
                              main: AmfArray,
                              other: AmfArray,
                              errorHandler: ErrorHandler): Unit = {

    val existing = main.values.flatMap { m =>
      val obj = m.asInstanceOf[DomainElement]
      obj.fields.entry(key.key).map(_.scalar.value -> obj)
    }.toMap // TODO value without key?

    other.values.foreach { o =>
      val obj = o.asInstanceOf[DomainElement]
      obj.fields.entry(key.key) match {
        case Some(value) =>
          if (existing.contains(value.scalar.value)) {
            merge(existing(value.scalar.value), obj.adopted(target.id), errorHandler)
          } else if (!isOptional(element, obj)) { // Case (2) -> If node is undefined in 'main' but is optional in 'other'.
            target.add(field, adoptInner(target.id, o))
          }
        case _ =>
      }
    }
  }

  private def isOptional(`type`: Type, obj: DomainElement) =
    `type`.isInstanceOf[OptionalField] && obj.fields
      .entry(`type`.asInstanceOf[OptionalField].Optional)
      .exists(_.scalar.toBool)

  private def ignored(entry: FieldEntry) = entry.field match {
    case Extends | Sources | LinkableElementModel.Target => false
    case _                                               => true
  }
}

/** Merge two data nodes of the same type. This merging applies the 'other' side as an overlay to the 'main' side. */
object DataNodeMerging {

  def merge(existing: DataNode, overlay: DataNode): Unit = {
    (existing, overlay) match {
      case (left: ScalarNode, right: ScalarNode) =>
        left.value = right.value
        left.dataType = right.dataType
      case (left: ObjectNode, right: ObjectNode) =>
        mergeObjectNode(left, right)
      case (left: ArrayNode, right: ArrayNode) =>
        // Add members that are not in the left array.
        mergeArrayNode(left, right)
      case _ =>
    }
  }

  def mergeObjectNode(left: ObjectNode, right: ObjectNode): Unit =
    for { (key, value) <- right.properties } {
      left.properties.get(key) match {
        case Some(property) => merge(property, value)
        case None           => left.addProperty(key, adoptTree(left.id, value), right.propertyAnnotations(key))
      }
    }

  /** Merge array data nodes by value: If scalar, check it's not there and add. If object or array, just add but adoptInner ids. */
  private def mergeArrayNode(main: ArrayNode, other: ArrayNode): Unit = {
    val existing = main.members.collect { case s: ScalarNode => s.value }

    other.members.foreach {
      case scalar: ScalarNode =>
        if (!existing.contains(scalar.value)) main.addMember(scalar)
      case node =>
        main.addMember(adoptTree(main.id, node))
    }
  }
}
