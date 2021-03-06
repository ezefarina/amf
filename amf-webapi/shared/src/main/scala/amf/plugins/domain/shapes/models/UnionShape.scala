package amf.plugins.domain.shapes.models

import amf.core.model.domain.{DomainElement, Linkable, Shape}
import amf.core.parser.{Annotations, Fields}
import amf.plugins.domain.shapes.metamodel.{AnyShapeModel, UnionShapeModel}
import amf.plugins.domain.shapes.metamodel.UnionShapeModel._
import org.yaml.model.YPart
import amf.core.utils.Strings

case class UnionShape(override val fields: Fields, override val annotations: Annotations)
    extends AnyShape(fields, annotations) {

  def anyOf: Seq[Shape] = fields.field(AnyOf)

  def withAnyOf(elements: Seq[Shape]): this.type = this.setArray(AnyOf, elements)

  override def linkCopy(): AnyShape = UnionShape().withId(id)

  override def meta: AnyShapeModel = UnionShapeModel

  /** Value , path + field value that is used to compose the id when the object its adopted */
  override def componentId: String = "/union/" + name.option().getOrElse("default-union").urlComponentEncoded

  def isPolymorphicUnion: Boolean = {
    anyOf.foldLeft(true) {
      case (acc, shape) =>
        acc && shape.isInstanceOf[AnyShape] && shape.asInstanceOf[AnyShape].supportsInheritance
    }
  }

  override def ramlSyntaxKey: String = "unionShape"

  /** apply method for create a new instance with fields and annotations. Aux method for copy */
  override protected def classConstructor: (Fields, Annotations) => Linkable with DomainElement = UnionShape.apply
}

object UnionShape {

  def apply(): UnionShape = apply(Annotations())

  def apply(ast: YPart): UnionShape = apply(Annotations(ast))

  def apply(annotations: Annotations): UnionShape = UnionShape(Fields(), annotations)

}
