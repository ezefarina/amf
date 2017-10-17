package amf.spec.oas

import amf.document.Fragment.{ExtensionFragment, OverlayFragment}
import amf.document.{BaseUnit, Document, Module}
import amf.domain.Annotation._
import amf.domain._
import amf.domain.extensions.{CustomDomainProperty, idCounter}
import amf.metadata.Field
import amf.metadata.domain._
import amf.metadata.domain.extensions.CustomDomainPropertyModel
import amf.metadata.shape._
import amf.model.{AmfArray, AmfScalar}
import amf.parser.Position.ZERO
import amf.parser.{ASTEmitter, Position}
import amf.remote.{Oas, Vendor}
import amf.shape._
import amf.spec._
import amf.spec.common.BaseSpecEmitter
import amf.vocabulary.VocabularyMappings
import org.yaml.model.YDocument.{EntryBuilder, PartBuilder}
import org.yaml.model.{YDocument, YType}

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * OpenAPI Spec Emitter.
  */
case class OasDocumentEmitter(document: BaseUnit) extends OasSpecEmitter {

  private def retrieveWebApi(): WebApi = document match {
    case document: Document           => document.encodes.asInstanceOf[WebApi]
    case extension: ExtensionFragment => extension.encodes
    case overlay: OverlayFragment     => overlay.encodes
    case _                            => throw new Exception("BaseUnit doesn't encode a WebApi.")
  }

  def emitDocument(): YDocument = {
    val doc = document.asInstanceOf[Document]

    val ordering = SpecOrdering.ordering(Oas, doc.encodes.annotations)

    val api        = emitWebApi(ordering)
    val declares   = DeclarationsEmitter(doc.declares, ordering).emitters
    val references = ReferencesEmitter(document.references, ordering)

    YDocument {
      _.map { b =>
        b.entry("swagger", "2.0")
        traverse(ordering.sorted(api ++ declares :+ references), b)
      }
    }
  }

  def emitWebApi(ordering: SpecOrdering): Seq[EntryEmitter] = {
    val model  = retrieveWebApi()
    val vendor = model.annotations.find(classOf[SourceVendor]).map(_.vendor)
    val api    = WebApiEmitter(model, ordering, vendor)
    api.emitters
  }

  case class WebApiEmitter(api: WebApi, ordering: SpecOrdering, vendor: Option[Vendor]) {
    val emitters: Seq[EntryEmitter] = {
      val fs     = api.fields
      val result = mutable.ListBuffer[EntryEmitter]()

      result += InfoEmitter(fs, ordering)

      fs.entry(WebApiModel.Host).map(f => result += ValueEmitter("host", f))

      fs.entry(WebApiModel.BaseUriParameters)
        .map(f => result += RamlParametersEmitter("x-base-uri-parameters", f, ordering))

      fs.entry(WebApiModel.BasePath).map(f => result += ValueEmitter("basePath", f))

      fs.entry(WebApiModel.Accepts)
        .map(f => result += ArrayEmitter("consumes", f, ordering))

      fs.entry(WebApiModel.ContentType)
        .map(f => result += ArrayEmitter("produces", f, ordering))

      fs.entry(WebApiModel.Schemes)
        .map(f => result += ArrayEmitter("schemes", f, ordering))

      fs.entry(WebApiModel.Provider).map(f => result += OrganizationEmitter("contact", f, ordering))

      fs.entry(WebApiModel.Documentation).map(f => result += CreativeWorkEmitter("externalDocs", f, ordering))

      fs.entry(WebApiModel.EndPoints).map(f => result += EndpointsEmitter("paths", f, ordering))

      result ++= OasAnnotationsEmitter(api, ordering).emitters

      ordering.sorted(result)
    }

    private case class InfoEmitter(fs: Fields, ordering: SpecOrdering) extends EntryEmitter {
      override def emit(b: EntryBuilder): Unit = {
        val result = mutable.ListBuffer[EntryEmitter]()

        fs.entry(WebApiModel.Name).map(f => result += ValueEmitter("title", f))

        fs.entry(WebApiModel.Description).map(f => result += ValueEmitter("description", f))

        fs.entry(WebApiModel.TermsOfService).map(f => result += ValueEmitter("termsOfService", f))

        fs.entry(WebApiModel.Version).map(f => result += ValueEmitter("version", f))

        fs.entry(WebApiModel.License).map(f => result += LicenseEmitter("license", f, ordering))

        if (result.nonEmpty)
          b.entry(
            "info",
            _.map(traverse(ordering.sorted(result), _))
          )
      }

      override def position(): Position = {
        var result: Position = ZERO
        fs.entry(WebApiModel.Version)
          .foreach(
            f =>
              f.value.annotations
                .find(classOf[LexicalInformation])
                .foreach({
                  case LexicalInformation(range) => result = range.start
                }))
        fs.entry(WebApiModel.Name)
          .foreach(
            f =>
              f.value.annotations
                .find(classOf[LexicalInformation])
                .foreach({
                  case LexicalInformation(range) =>
                    if (result.isZero || range.start.lt(result)) {
                      result = range.start
                    }
                }))
        result
      }
    }

  }

  case class EndPointEmitter(endpoint: EndPoint, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      val fs = endpoint.fields

      sourceOr(
        endpoint.annotations,
        b.complexEntry(
          ScalarEmitter(fs.entry(EndPointModel.Path).get.scalar).emit(_),
          _.map { b =>
            val result = mutable.ListBuffer[EntryEmitter]()

            fs.entry(EndPointModel.Name).map(f => result += ValueEmitter("displayName", f))
            fs.entry(EndPointModel.Description).map(f => result += ValueEmitter("description", f))
            fs.entry(DomainElementModel.Extends).map(f => result ++= ExtendsEmitter("x-", f, ordering).emitters())

            val parameters = endPointParameters()

            if (parameters.nonEmpty)
              result += ParametersEmitter("parameters", parameters.parameters(), ordering, parameters.body)

            fs.entry(EndPointModel.Operations).map(f => result ++= operations(f, ordering, parameters.body.isDefined))

            result ++= OasAnnotationsEmitter(endpoint, ordering).emitters

            traverse(ordering.sorted(result), b)
          }
        )
      )
    }

    private def endPointParameters(): EndPointParameters =
      endpoint.operations
        .filter(op => Option(op.request).isDefined)
        .foldLeft(EndPointParameters(path = endpoint.parameters))((parameters, op) =>
          parameters.merge(EndPointParameters(op.request)))

    private def operations(f: FieldEntry, ordering: SpecOrdering, endpointPayloadEmitted: Boolean): Seq[EntryEmitter] =
      f.array.values
        .map(e => OperationEmitter(e.asInstanceOf[Operation], ordering, endpointPayloadEmitted))

    override def position(): Position = pos(endpoint.annotations)
  }

  case class ParametersEmitter(key: String,
                               parameters: Seq[Parameter],
                               ordering: SpecOrdering,
                               payloadOption: Option[Payload] = None)
      extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      b.entry(
        key,
        _.list(traverse(parameters(ordering), _))
      )
    }

    private def parameters(ordering: SpecOrdering): Seq[PartEmitter] = {
      val result = mutable.ListBuffer[PartEmitter]()
      parameters.foreach(e => result += ParameterEmitter(e, ordering))

      payloadOption.foreach(payload => result += PayloadAsParameterEmitter(payload, ordering))

      ordering.sorted(result)
    }

    override def position(): Position = {
      if (parameters.nonEmpty) pos(parameters.head.annotations)
      else payloadOption.fold[Position](ZERO)(payload => pos(payload.annotations))
    }
  }

  case class PayloadAsParameterEmitter(payload: Payload, ordering: SpecOrdering) extends PartEmitter {
    override def position(): Position = pos(payload.annotations)

    override def emit(b: PartBuilder): Unit = {
      b.map { b =>
        val result = mutable.ListBuffer[EntryEmitter]()

        payload.fields
          .entry(PayloadModel.Schema)
          .map(f => result += SchemaEmitter(f, ordering))

        payload.fields.entry(PayloadModel.MediaType).map(f => result += ValueEmitter("x-media-type", f))

        result += MapEntryEmitter("in", "body")

        result ++= OasAnnotationsEmitter(payload, ordering).emitters

        traverse(ordering.sorted(result), b)
      }
    }
  }

  case class OperationEmitter(operation: Operation, ordering: SpecOrdering, endpointPayloadEmitted: Boolean)
      extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      val fs = operation.fields

      sourceOr(
        operation.annotations,
        b.complexEntry(
          ScalarEmitter(fs.entry(OperationModel.Method).get.scalar).emit(_),
          _.map { b =>
            val result = mutable.ListBuffer[EntryEmitter]()

            fs.entry(OperationModel.Name).map(f => result += ValueEmitter("operationId", f))
            fs.entry(OperationModel.Description).map(f => result += ValueEmitter("description", f))
            fs.entry(OperationModel.Deprecated).map(f => result += ValueEmitter("deprecated", f, YType.Bool))
            fs.entry(OperationModel.Summary).map(f => result += ValueEmitter("summary", f))
            fs.entry(OperationModel.Documentation).map(f => result += CreativeWorkEmitter("externalDocs", f, ordering))
            fs.entry(OperationModel.Schemes).map(f => result += ArrayEmitter("schemes", f, ordering))
            fs.entry(OperationModel.Accepts).map(f => result += ArrayEmitter("consumes", f, ordering))
            fs.entry(OperationModel.ContentType).map(f => result += ArrayEmitter("produces", f, ordering))
            fs.entry(DomainElementModel.Extends).map(f => result ++= ExtendsEmitter("x-", f, ordering).emitters())

            Option(operation.request).foreach(req => result ++= requestEmitters(req, ordering, endpointPayloadEmitted))

            fs.entry(OperationModel.Responses).map(f => result += ResponsesEmitter("responses", f, ordering))

            result ++= OasAnnotationsEmitter(operation, ordering).emitters

            traverse(ordering.sorted(result), b)
          }
        )
      )
    }

    override def position(): Position = pos(operation.annotations)

    def requestEmitters(request: Request, ordering: SpecOrdering, endpointPayloadEmitted: Boolean): Seq[EntryEmitter] = {

      val result = mutable.ListBuffer[EntryEmitter]()

      val parameters = operationOnly(request.queryParameters) ++ operationOnly(request.headers)
      val payloads   = Payloads(request.payloads, endpointPayloadEmitted)

      if (parameters.nonEmpty || payloads.default.isDefined)
        result += ParametersEmitter("parameters", parameters, ordering, payloads.default)

      if (payloads.other.nonEmpty) result += PayloadsEmitter("x-request-payloads", payloads.other, ordering)

      result ++= OasAnnotationsEmitter(request, ordering).emitters

      result
    }

    private def operationOnly(parameters: Seq[Parameter]) =
      parameters.filter(!_.annotations.contains(classOf[Annotation.EndPointParameter]))

  }

  case class ResponsesEmitter(key: String, f: FieldEntry, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      sourceOr(
        f.value.annotations,
        b.entry(
          key,
          _.map(traverse(responses(f, ordering), _))
        )
      )
    }

    private def responses(f: FieldEntry, ordering: SpecOrdering): Seq[EntryEmitter] = {
      ordering.sorted(f.array.values.map(e => ResponseEmitter(e.asInstanceOf[Response], ordering)))
    }

    override def position(): Position = pos(f.value.annotations)
  }

  case class ResponseEmitter(response: Response, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      val fs = response.fields

      sourceOr(
        response.annotations,
        b.complexEntry(
          ScalarEmitter(fs.entry(ResponseModel.Name).get.scalar).emit(_),
          _.map { b =>
            val result = mutable.ListBuffer[EntryEmitter]()

            fs.entry(ResponseModel.Description).map(f => result += ValueEmitter("description", f))
            fs.entry(RequestModel.Headers).map(f => result += RamlParametersEmitter("headers", f, ordering))

            val payloads = Payloads(response.payloads)

            payloads.default.foreach(payload => {
              payload.fields.entry(PayloadModel.MediaType).map(f => result += ValueEmitter("x-media-type", f))
              payload.fields
                .entry(PayloadModel.Schema)
                .map(f => result += SchemaEmitter(f, ordering))
            })

            if (payloads.other.nonEmpty)
              result += PayloadsEmitter("x-response-payloads", payloads.other, ordering)

            result ++= OasAnnotationsEmitter(response, ordering).emitters

            traverse(ordering.sorted(result), b)
          }
        )
      )
    }

    override def position(): Position = pos(response.annotations)
  }

  case class PayloadsEmitter(key: String, payloads: Seq[Payload], ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      b.entry(
        key,
        _.list(traverse(ordering.sorted(payloads.map(p => PayloadEmitter(p, ordering))), _))
      )
    }

    override def position(): Position = {
      val filtered = payloads
        .filter(p => p.annotations.find(classOf[LexicalInformation]).exists(!_.range.start.isZero))
      val result = filtered
        .foldLeft[Position](ZERO)(
          (pos, p) =>
            p.annotations
              .find(classOf[LexicalInformation])
              .map(_.range.start)
              .filter(newPos => pos.isZero || pos.lt(newPos))
              .getOrElse(pos))
      result
    }
  }

  case class PayloadEmitter(payload: Payload, ordering: SpecOrdering) extends PartEmitter {
    override def emit(b: PartBuilder): Unit = {
      sourceOr(
        payload.annotations,
        b.map { b =>
          val fs     = payload.fields
          val result = mutable.ListBuffer[EntryEmitter]()

          fs.entry(PayloadModel.MediaType).map(f => result += ValueEmitter("mediaType", f))
          fs.entry(PayloadModel.Schema).map(f => result += SchemaEmitter(f, ordering))

          result ++= OasAnnotationsEmitter(payload, ordering).emitters

          traverse(ordering.sorted(result), b)
        }
      )
    }

    override def position(): Position = pos(payload.annotations)
  }

  case class RamlParametersEmitter(key: String, f: FieldEntry, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      sourceOr(
        f.value.annotations,
        b.entry(
          key,
          _.map { b =>
            traverse(parameters(f, ordering), b)
          }
        )
      )
    }

    private def parameters(f: FieldEntry, ordering: SpecOrdering): Seq[EntryEmitter] = {
      val result = mutable.ListBuffer[EntryEmitter]()
      f.array.values
        .foreach(e => result += RamlParameterEmitter(e.asInstanceOf[Parameter], ordering))
      ordering.sorted(result)
    }

    override def position(): Position = pos(f.value.annotations)
  }

  case class ParameterEmitter(parameter: Parameter, ordering: SpecOrdering) extends PartEmitter {
    override def emit(b: PartBuilder): Unit = {
      sourceOr(
        parameter.annotations,
        b.map { b =>
          val result = mutable.ListBuffer[EntryEmitter]()
          val fs     = parameter.fields

          fs.entry(ParameterModel.Name).map(f => result += ValueEmitter("name", f))

          fs.entry(ParameterModel.Description).map(f => result += ValueEmitter("description", f))

          fs.entry(ParameterModel.Required)
            .filter(_.value.annotations.contains(classOf[ExplicitField]) || parameter.required)
            .map(f => result += ValueEmitter("required", f, YType.Bool))

          fs.entry(ParameterModel.Binding).map(f => result += ValueEmitter("in", f))

          fs.entry(ParameterModel.Schema)
            .map(f =>
              result ++= OasTypeEmitter(f.value.value.asInstanceOf[Shape], ordering, Seq(ShapeModel.Description))
                .emitters())

          result ++= OasAnnotationsEmitter(parameter, ordering).emitters

          traverse(ordering.sorted(result), b)
        }
      )
    }

    override def position(): Position = pos(parameter.annotations)
  }

  case class EndpointsEmitter(key: String, f: FieldEntry, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      sourceOr(
        f.value.annotations,
        b.entry(
          key,
          _.map(b => traverse(endpoints(f, ordering), b))
        )
      )
    }

    private def endpoints(f: FieldEntry, ordering: SpecOrdering): Seq[EntryEmitter] = {
      val result = f.array.values.map(e => EndPointEmitter(e.asInstanceOf[EndPoint], ordering))
      ordering.sorted(result)
    }

    override def position(): Position = pos(f.value.annotations)
  }

  case class RamlParameterEmitter(parameter: Parameter, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      val fs = parameter.fields

      sourceOr(
        parameter.annotations,
        b.complexEntry(
          ScalarEmitter(fs.entry(ParameterModel.Name).get.scalar).emit(_),
          _.map { b =>
            val result = mutable.ListBuffer[EntryEmitter]()

            fs.entry(ParameterModel.Description).map(f => result += ValueEmitter("description", f))

            fs.entry(ParameterModel.Required)
              .filter(_.value.annotations.contains(classOf[ExplicitField]))
              .map(f => result += ValueEmitter("required", f, YType.Bool))

            fs.entry(ParameterModel.Schema)
              .map(f =>
                result ++= OasTypeEmitter(f.value.value.asInstanceOf[Shape], ordering, Seq(ShapeModel.Description))
                  .emitters())

            traverse(ordering.sorted(result), b)
          }
        )
      )
    }

    override def position(): Position = pos(parameter.annotations)
  }

  case class LicenseEmitter(key: String, f: FieldEntry, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      sourceOr(
        f.value,
        b.entry(
          key,
          _.map { b =>
            val fs     = f.obj.fields
            val result = mutable.ListBuffer[EntryEmitter]()

            fs.entry(LicenseModel.Url).map(f => result += ValueEmitter("url", f))
            fs.entry(LicenseModel.Name).map(f => result += ValueEmitter("name", f))

            result ++= OasAnnotationsEmitter(f.domainElement, ordering).emitters

            traverse(ordering.sorted(result), b)
          }
        )
      )
    }

    override def position(): Position = pos(f.value.annotations)
  }

  case class OrganizationEmitter(key: String, f: FieldEntry, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      sourceOr(
        f.value,
        b.entry(
          key,
          _.map { b =>
            val fs     = f.obj.fields
            val result = mutable.ListBuffer[EntryEmitter]()

            fs.entry(OrganizationModel.Url).map(f => result += ValueEmitter("url", f))
            fs.entry(OrganizationModel.Name).map(f => result += ValueEmitter("name", f))
            fs.entry(OrganizationModel.Email).map(f => result += ValueEmitter("email", f))

            result ++= OasAnnotationsEmitter(f.domainElement, ordering).emitters

            traverse(ordering.sorted(result), b)
          }
        )
      )
    }

    override def position(): Position = pos(f.value.annotations)
  }

  case class EndPointParameters(query: Seq[Parameter] = Nil,
                                path: Seq[Parameter] = Nil,
                                header: Seq[Parameter] = Nil,
                                body: Option[Payload] = None) {

    def merge(parameters: EndPointParameters): EndPointParameters = {
      EndPointParameters(merge(query, parameters.query),
                         merge(path, parameters.path),
                         merge(header, parameters.header),
                         merge(body, parameters.body))
    }

    private def merge(left: Seq[Parameter], right: Seq[Parameter]): Seq[Parameter] =
      (endPointOnly(left) ++ endPointOnly(right)).values.toSeq

    private def merge(left: Option[Payload], right: Option[Payload]): Option[Payload] = left.fold(right)(Some(_))

    private def endPointOnly(left: Seq[Parameter]): Map[String, Parameter] = {
      left.filter(p => p.annotations.contains(classOf[EndPointParameter]) || p.isPath).map(p => p.name -> p).toMap
    }

    def parameters(): Seq[Parameter] = query ++ path ++ header

    def nonEmpty: Boolean = query.nonEmpty || path.nonEmpty || header.nonEmpty || body.isDefined
  }

  object EndPointParameters {
    def apply(request: Request): EndPointParameters = {
      EndPointParameters(request.queryParameters,
                         Nil,
                         request.headers,
                         request.payloads.find(_.annotations.contains(classOf[EndPointBodyParameter])))
    }
  }

  case class Payloads(default: Option[Payload], other: Seq[Payload])

  object Payloads {
    def apply(payloads: Seq[Payload], endpointPayloadEmitted: Boolean = false): Payloads = {
      val clean = payloads.filter(!_.annotations.contains(classOf[EndPointBodyParameter]))

      var default = clean.find(_.annotations.contains(classOf[DefaultPayload]))

      default = if (endpointPayloadEmitted) default else default.orElse(defaultPayload(clean))

      Payloads(default, clean.filter(_ != default.orNull))
    }

    def defaultPayload(payloads: Seq[Payload]): Option[Payload] =
      payloads
        .find(p => Option(p.mediaType).isEmpty || p.mediaType.isEmpty)
        .orElse(payloads.find(_.mediaType == "application/json"))
        .orElse(payloads.headOption)
  }

}

class OasSpecEmitter extends BaseSpecEmitter {
  val emitter = ASTEmitter()

  case class ReferencesEmitter(references: Seq[BaseUnit], ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      val modules = references.collect({ case m: Module => m })
      if (modules.nonEmpty) {
        b.entry(
          "x-uses",
          _.map { b =>
            idCounter.reset()
            traverse(ordering.sorted(references.map(r => ReferenceEmitter(r, ordering, idCounter.genId("uses")))), b)
          }
        )
      }
    }

    override def position(): Position = ZERO
  }

  case class ReferenceEmitter(reference: BaseUnit, ordering: SpecOrdering, alias: String) extends EntryEmitter {

    override def emit(b: EntryBuilder): Unit = MapEntryEmitter(alias, reference.id).emit(b)

    override def position(): Position = ZERO

  }

  case class DeclarationsEmitter(declares: Seq[DomainElement], ordering: SpecOrdering) {
    val emitters: Seq[EntryEmitter] = {
      val declarations = Declarations(declares)

      val result = ListBuffer[EntryEmitter]()

      if (declarations.shapes.nonEmpty) result += DeclaredTypesEmitters(declarations.shapes.values.toSeq, ordering)

      if (declarations.annotations.nonEmpty)
        result += AnnotationsTypesEmitter(declarations.annotations.values.toSeq, ordering)

      if (declarations.resourceTypes.nonEmpty)
        result += AbstractDeclarationsEmitter("x-resourceTypes",
                                              declarations.resourceTypes.values.toSeq,
                                              ordering,
                                              (e: DomainElement, key: String) => TagToReferenceEmitter(e, Some(key)))

      if (declarations.traits.nonEmpty)
        result += AbstractDeclarationsEmitter("x-traits",
                                              declarations.traits.values.toSeq,
                                              ordering,
                                              (e: DomainElement, key: String) => TagToReferenceEmitter(e, Some(key)))

      result
    }
  }

  case class DeclaredTypesEmitters(types: Seq[Shape], ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      b.entry("definitions", _.map { b =>
        traverse(ordering.sorted(types.map(NamedTypeEmitter(_, ordering))), b)
      })
    }
    override def position(): Position = types.headOption.map(a => pos(a.annotations)).getOrElse(ZERO)
  }

  case class NamedTypeEmitter(shape: Shape, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      val name = Option(shape.name).getOrElse(throw new Exception(s"Cannot declare shape without name $shape"))
      b.entry(name, if (shape.isLink) emitLink _ else emitLocalType _)
    }

    private def emitLink(b: PartBuilder) = TagToReferenceEmitter(shape, shape.linkLabel).emit(b)

    private def emitLocalType(b: PartBuilder) = {
      b.map(traverse(ordering.sorted(OasTypeEmitter(shape, ordering).emitters()), _))
    }

    override def position(): Position = pos(shape.annotations)
  }

  case class TagToReferenceEmitter(target: DomainElement, label: Option[String]) extends PartEmitter {
    override def emit(b: PartBuilder): Unit = {
      val reference = label.getOrElse(target.id)
      map { () =>
        follow() match {
          case s: Shape if s.annotations.contains(classOf[DeclaredElement]) =>
            ref(b, appendDefinitionsPrefix(reference))
          case _ => ref(b, reference)
        }
      }
    }

    /** Follow links. */
    private def follow(): DomainElement = {
      target match {
        case s: Linkable if s.isLink =>
          s.linkTarget match {
            case Some(t) => t
            case _       => throw new Exception(s"Expected shape link target on $target")
          }
        case other => other
      }
    }

    override def position(): Position = pos(target.annotations)
  }

  case class NamedRefEmitter(key: String, url: String, pos: Position = ZERO) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      b.entry(
        key,
        ref(_, url)
      )
    }

    override def position(): Position = pos
  }

  protected def ref(b: PartBuilder, url: String): Unit = b.map(MapEntryEmitter("$ref", url).emit(_))

  case class AnnotationsTypesEmitter(properties: Seq[CustomDomainProperty], ordering: SpecOrdering)
      extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      b.entry("x-annotationTypes", _.map { b =>
        traverse(ordering.sorted(properties.map(NamedPropertyTypeEmitter(_, ordering))), b)
      })
    }

    override def position(): Position = properties.headOption.map(p => pos(p.annotations)).getOrElse(ZERO)
  }

  case class NamedPropertyTypeEmitter(annotationType: CustomDomainProperty, ordering: SpecOrdering)
      extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      b.entry(
        Option(annotationType.name)
          .orElse(throw new Exception(s"Cannot declare annotation type without name $annotationType"))
          .get,
        b => {
          if (annotationType.isLink) TagToReferenceEmitter(annotationType, annotationType.linkLabel).emit(b)
          else
            b.map { b =>
              val emitters = AnnotationTypeEmitter(annotationType, ordering).emitters()
              traverse(ordering.sorted(emitters), b)
            }
        }
      )
    }

    def emitAnnotationFields(): Unit = {}

    override def position(): Position = pos(annotationType.annotations)
  }

  case class OasTypeEmitter(shape: Shape, ordering: SpecOrdering, ignored: Seq[Field] = Nil) {
    def emitters(): Seq[EntryEmitter] = {
      shape match {
//        case l: Linkable if l.isLink => Seq(TagToReferenceEmitter(shape, l.linkLabel))
        case l: Linkable if l.isLink => {
          println("Grrrrr")
          ???
        }
        case any: AnyShape =>
          val copiedNode = any.copy(fields = any.fields.filter(f => !ignored.contains(f._1))) // node (amf object) id get loses
          Seq(AnyShapeEmitter(copiedNode, ordering))
        case node: NodeShape =>
          val copiedNode = node.copy(fields = node.fields.filter(f => !ignored.contains(f._1))) // node (amf object) id get loses
          NodeShapeEmitter(copiedNode, ordering).emitters()
        case union: UnionShape =>
          val copiedNode = union.copy(fields = union.fields.filter(f => !ignored.contains(f._1)))
          Seq(UnionShapeEmitter(copiedNode, ordering))
        case array: ArrayShape =>
          val copiedArray = array.copy(fields = array.fields.filter(f => !ignored.contains(f._1)))
          ArrayShapeEmitter(copiedArray, ordering).emitters()
        case nil: NilShape =>
          val copiedNil = nil.copy(fields = nil.fields.filter(f => !ignored.contains(f._1)))
          Seq(NilShapeEmitter(copiedNil, ordering))
        case file: FileShape =>
          val copiedScalar = file.copy(fields = file.fields.filter(f => !ignored.contains(f._1)))
          FileShapeEmitter(copiedScalar, ordering).emitters()
        case scalar: ScalarShape =>
          val copiedScalar = scalar.copy(fields = scalar.fields.filter(f => !ignored.contains(f._1)))
          ScalarShapeEmitter(copiedScalar, ordering).emitters()
        case _ => Seq()
      }
    }
  }

  abstract class ShapeEmitter(shape: Shape, ordering: SpecOrdering) {
    def emitters(): Seq[EntryEmitter] = {

      val result = ListBuffer[EntryEmitter]()
      val fs     = shape.fields

      fs.entry(ShapeModel.DisplayName).map(f => result += ValueEmitter("title", f))

      fs.entry(ShapeModel.Description).map(f => result += ValueEmitter("description", f))

      fs.entry(ShapeModel.Default).map(f => result += ValueEmitter("default", f))

      fs.entry(ShapeModel.Values).map(f => result += ArrayEmitter("enum", f, ordering))

      fs.entry(ShapeModel.Documentation).map(f => result += CreativeWorkEmitter("externalDocs", f, ordering))

      fs.entry(ShapeModel.XMLSerialization).map(f => result += XMLSerializerEmitter("xml", f, ordering))

      result ++= OasAnnotationsEmitter(shape, ordering).emitters

      result
    }
  }

  case class UnionShapeEmitter(shape: UnionShape, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {

      b.entry(
        "anyOf",
        _.list { b =>
          val emitters = shape.anyOf
            .map { shape =>
              ordering.sorted(OasTypeEmitter(shape, ordering).emitters())
            }
            .map { emitters =>
              new EntryEmitter {
                override def position(): Position        = emitters.head.position()
                override def emit(b: EntryBuilder): Unit = emitters.foreach(_.emit(b))
              }
            }
          ordering.sorted(emitters).foreach { emitter =>
            b.map { emitter.emit }
          }
        }
      )
    }

    override def position(): Position = pos(shape.annotations)
  }

  case class AnyShapeEmitter(shape: Shape, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      // ignore
    }
    override def position(): Position = pos(shape.annotations)
  }

  case class ArrayShapeEmitter(shape: ArrayShape, ordering: SpecOrdering) {
    def emitters(): Seq[EntryEmitter] = {
      val result = ListBuffer[EntryEmitter]()
      val fs     = shape.fields

      result += MapEntryEmitter("type", "array")

      result += ItemsShapeEmitter(shape, ordering)

      fs.entry(ArrayShapeModel.MaxItems).map(f => result += ValueEmitter("maxItems", f))

      fs.entry(ArrayShapeModel.MinItems).map(f => result += ValueEmitter("minItems", f))

      fs.entry(ArrayShapeModel.UniqueItems).map(f => result += ValueEmitter("uniqueItems", f))

      result ++= OasAnnotationsEmitter(shape, ordering).emitters

      result
    }
  }

  case class ItemsShapeEmitter(array: ArrayShape, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      b.entry(
        "items",
        // todo syaml review map or array ??
        _.map(b => OasTypeEmitter(array.items, ordering).emitters().foreach(e => e.emit(b)))
      )
    }

    override def position(): Position = pos(array.items.fields.getValue(ArrayShapeModel.Items).annotations)
  }

  case class XMLSerializerEmitter(key: String, f: FieldEntry, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      sourceOr(
        f.value,
        b.entry(
          key,
          _.map { b =>
            val fs     = f.obj.fields
            val result = mutable.ListBuffer[EntryEmitter]()

            fs.entry(XMLSerializerModel.Attribute)
              .filter(_.value.annotations.contains(classOf[ExplicitField]))
              .map(f => result += ValueEmitter("attribute", f))

            fs.entry(XMLSerializerModel.Wrapped)
              .filter(_.value.annotations.contains(classOf[ExplicitField]))
              .map(f => result += ValueEmitter("wrapped", f))

            fs.entry(XMLSerializerModel.Name)
              .filter(_.value.annotations.contains(classOf[ExplicitField]))
              .map(f => result += ValueEmitter("name", f))

            fs.entry(XMLSerializerModel.Namespace).map(f => result += ValueEmitter("namespace", f))

            fs.entry(XMLSerializerModel.Prefix).map(f => result += ValueEmitter("prefix", f))

            result ++= OasAnnotationsEmitter(f.domainElement, ordering).emitters

            traverse(ordering.sorted(result), b)
          }
        )
      )
    }

    override def position(): Position = pos(f.value.annotations)
  }

  case class NodeShapeEmitter(node: NodeShape, ordering: SpecOrdering) extends ShapeEmitter(node, ordering) {
    override def emitters(): Seq[EntryEmitter] = {
      val result: ListBuffer[EntryEmitter] = ListBuffer(super.emitters(): _*)

      val fs = node.fields

      // TODO annotation for original position?
      if (node.annotations.contains(classOf[ExplicitField]))
        result += MapEntryEmitter("type", "object")

      fs.entry(NodeShapeModel.MinProperties).map(f => result += ValueEmitter("minProperties", f))

      fs.entry(NodeShapeModel.MaxProperties).map(f => result += ValueEmitter("maxProperties", f))

      fs.entry(NodeShapeModel.Closed)
        .filter(_.value.annotations.contains(classOf[ExplicitField]))
        .map(
          f =>
            result += MapEntryEmitter("additionalProperties",
                                      (!node.closed).toString,
                                      position = pos(f.value.annotations)))

      fs.entry(NodeShapeModel.Discriminator).map(f => result += ValueEmitter("discriminator", f))

      fs.entry(NodeShapeModel.DiscriminatorValue).map(f => result += ValueEmitter("x-discriminator-value", f))

      fs.entry(NodeShapeModel.ReadOnly).map(f => result += ValueEmitter("readOnly", f))

      // TODO required array.

      fs.entry(NodeShapeModel.Properties).map(f => result += PropertiesShapeEmitter(f, ordering))

      val properties = ListMap(node.properties.map(p => p.id -> p): _*)

      fs.entry(NodeShapeModel.Dependencies).map(f => result += ShapeDependenciesEmitter(f, ordering, properties))

      fs.entry(NodeShapeModel.Inherits).map(f => result += ShapeInheritsEmitter(f, ordering))

      result
    }

  }

  case class ShapeInheritsEmitter(f: FieldEntry, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      val inherits = f.array.values.map(_.asInstanceOf[Shape])
      b.entry(
        "allOf",
        _.list(b =>
          inherits.foreach { s =>
            if (s.annotations.contains(classOf[DeclaredElement])) {
              b.map(traverse(ordering.sorted(OasTypeEmitter(s, ordering).emitters()), _))
            } else ref(b, appendDefinitionsPrefix(s.name))
        })
      )
    }

    override def position(): Position = pos(f.value.annotations)
  }

  case class ShapeDependenciesEmitter(f: FieldEntry,
                                      ordering: SpecOrdering,
                                      propertiesMap: ListMap[String, PropertyShape])
      extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {

      b.entry(
        "dependencies",
        _.map { b =>
          val result = f.array.values.map(v =>
            PropertyDependenciesEmitter(v.asInstanceOf[PropertyDependencies], ordering, propertiesMap))
          traverse(ordering.sorted(result), b)
        }
      )
    }

    override def position(): Position = pos(f.value.annotations)
  }

  case class PropertyDependenciesEmitter(property: PropertyDependencies,
                                         ordering: SpecOrdering,
                                         properties: ListMap[String, PropertyShape])
      extends EntryEmitter {

    override def emit(b: EntryBuilder): Unit = {
      properties
        .get(property.propertySource)
        .foreach(p => {
          b.entry(
            p.name,
            _.list { b =>
              val targets = property.fields
                .entry(PropertyDependenciesModel.PropertyTarget)
                .map(f => {
                  f.array.scalars.flatMap(iri =>
                    properties.get(iri.value.toString).map(p => AmfScalar(p.name, iri.annotations)))
                })

              targets.foreach(target => {
                traverse(ordering.sorted(target.map(t => ScalarEmitter(t))), b)
              })
            }
          )
        })
    }

    override def position(): Position = pos(property.annotations) // TODO check this
  }

  case class NilShapeEmitter(nil: NilShape, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = b.entry("type", "null")

    override def position(): Position = pos(nil.annotations)
  }

  trait CommonOASFieldsEmitter {
    def emitCommonFields(fs: Fields, result: ListBuffer[EntryEmitter]): Option[result.type] = {
      fs.entry(ScalarShapeModel.Pattern).map(f => result += ValueEmitter("pattern", f))

      fs.entry(ScalarShapeModel.MinLength).map(f => result += ValueEmitter("minLength", f))

      fs.entry(ScalarShapeModel.MaxLength).map(f => result += ValueEmitter("maxLength", f))

      fs.entry(ScalarShapeModel.Minimum).map(f => result += ValueEmitter("minimum", f))

      fs.entry(ScalarShapeModel.Maximum).map(f => result += ValueEmitter("maximum", f))

      fs.entry(ScalarShapeModel.ExclusiveMinimum).map(f => result += ValueEmitter("exclusiveMinimum", f))

      fs.entry(ScalarShapeModel.ExclusiveMaximum).map(f => result += ValueEmitter("exclusiveMaximum", f))

      fs.entry(ScalarShapeModel.MultipleOf).map(f => result += ValueEmitter("multipleOf", f))

      fs.entry(ScalarShapeModel.Format).map(f => result += ValueEmitter("format", f))
    }
  }

  case class ScalarShapeEmitter(scalar: ScalarShape, ordering: SpecOrdering)
      extends ShapeEmitter(scalar, ordering)
      with CommonOASFieldsEmitter {
    override def emitters(): Seq[EntryEmitter] = {
      val result: ListBuffer[EntryEmitter] = ListBuffer(super.emitters(): _*)

      val fs = scalar.fields

      val typeDef = OasTypeDefStringValueMatcher.matchType(TypeDefXsdMapping.typeDef(scalar.dataType)) // TODO Check this

      fs.entry(ScalarShapeModel.DataType)
        .map(
          f =>
            result += MapEntryEmitter(
              "type",
              typeDef,
              position =
                if (f.value.annotations.contains(classOf[Inferred])) ZERO
                else pos(f.value.annotations))) // TODO check this  - annotations of typeDef in parser

      emitCommonFields(fs, result)

      result
    }
  }

  case class FileShapeEmitter(scalar: FileShape, ordering: SpecOrdering)
      extends ShapeEmitter(scalar, ordering)
      with CommonOASFieldsEmitter {
    override def emitters(): Seq[EntryEmitter] = {
      val result: ListBuffer[EntryEmitter] = ListBuffer(super.emitters(): _*)

      val fs = scalar.fields

      result += MapEntryEmitter("type", "file")

      emitCommonFields(fs, result)

      fs.entry(FileShapeModel.FileTypes).map(f => result += ArrayEmitter("x-fileTypes", f, ordering))

      result
    }
  }

  case class PropertiesShapeEmitter(f: FieldEntry, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {

      b.entry(
        "properties",
        _.map { b =>
          val result = f.array.values.map(v => PropertyShapeEmitter(v.asInstanceOf[PropertyShape], ordering))
          traverse(ordering.sorted(result), b)
        }
      )
    }

    override def position(): Position = pos(f.value.annotations)
  }

  case class PropertyShapeEmitter(property: PropertyShape, ordering: SpecOrdering) extends EntryEmitter {

    override def emit(b: EntryBuilder): Unit = {
      b.entry(
        property.name,
        _.map { b =>
          val emitters = ordering.sorted(OasTypeEmitter(property.range, ordering).emitters())

          if (emitters.nonEmpty) {
            emitters.head match {
              case e: TagToReferenceEmitter if emitters.size == 1 => e.emit(b)
              case _                                              => traverse(emitters, b)
            }
          }
        }
      )
    }

    override def position(): Position = pos(property.annotations) // TODO check this
  }

  case class AnnotationTypeEmitter(property: CustomDomainProperty, ordering: SpecOrdering) {
    def emitters(): Seq[EntryEmitter] = {
      val result = ListBuffer[EntryEmitter]()
      val fs     = property.fields

      fs.entry(CustomDomainPropertyModel.DisplayName).map(f => result += ValueEmitter("displayName", f))

      fs.entry(CustomDomainPropertyModel.Description).map(f => result += ValueEmitter("description", f))

      fs.entry(CustomDomainPropertyModel.Domain).map { f =>
        val scalars = f.array.scalars.map { s =>
          VocabularyMappings.uriToRaml.get(s.toString) match {
            case Some(identifier) => AmfScalar(identifier, s.annotations)
            case None             => s
          }
        }
        val finalArray      = AmfArray(scalars, f.array.annotations)
        val finalFieldEntry = FieldEntry(f.field, Value(finalArray, f.value.annotations))

        result += ArrayEmitter("allowedTargets", finalFieldEntry, ordering)
      }

      fs.entry(CustomDomainPropertyModel.Schema)
        .map({ f =>
          result += SchemaEmitter(f, ordering)
        })

      result ++= OasAnnotationsEmitter(property, ordering).emitters

      result
    }
  }

  case class SchemaEmitter(f: FieldEntry, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      val shape = f.value.value.asInstanceOf[Shape]

      b.entry(
        "schema",
        _.map { b =>
          val emitters = ordering.sorted(OasTypeEmitter(shape, ordering).emitters())

          if (emitters.nonEmpty) {
            emitters.head match {
              case e: TagToReferenceEmitter if emitters.size == 1 => e.emit(b)
              case _                                              => traverse(emitters, b)
            }
          }
        }
      )
    }

    override def position(): Position = pos(f.value.annotations)
  }

  case class CreativeWorkEmitter(key: String, f: FieldEntry, ordering: SpecOrdering) extends EntryEmitter {
    override def emit(b: EntryBuilder): Unit = {
      sourceOr(
        f.value,
        b.entry(
          key,
          _.map { b =>
            val fs     = f.obj.fields
            val result = mutable.ListBuffer[EntryEmitter]()

            fs.entry(CreativeWorkModel.Url).map(f => result += ValueEmitter("url", f))
            fs.entry(CreativeWorkModel.Description).map(f => result += ValueEmitter("description", f))

            result ++= OasAnnotationsEmitter(f.domainElement, ordering).emitters

            traverse(ordering.sorted(result), b)
          }
        )
      )
    }

    override def position(): Position = pos(f.value.annotations)
  }
}
