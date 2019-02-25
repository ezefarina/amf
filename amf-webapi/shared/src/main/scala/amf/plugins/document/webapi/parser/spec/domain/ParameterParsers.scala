package amf.plugins.document.webapi.parser.spec.domain

import amf.core.annotations.{ExplicitField, LexicalInformation, SynthesizedField}
import amf.core.metamodel.domain.ShapeModel
import amf.core.metamodel.domain.extensions.PropertyShapeModel
import amf.core.model.domain.{AmfScalar, DomainElement, NamedDomainElement, Shape}
import amf.core.parser.{Annotations, _}
import amf.core.utils.Strings
import amf.core.validation.core.ValidationSpecification
import amf.plugins.document.webapi.annotations.{
  FormBodyParameter,
  Inferred,
  ParameterNameForPayload,
  RequiredParamPayload
}
import amf.plugins.document.webapi.contexts.{OasWebApiContext, RamlWebApiContext, WebApiContext}
import amf.plugins.document.webapi.parser.spec.common.{AnnotationParser, SpecParserOps}
import amf.plugins.document.webapi.parser.spec.declaration.{
  Raml08TypeParser,
  Raml10TypeParser,
  RamlTypeSyntax,
  StringDefaultType,
  _
}
import amf.plugins.document.webapi.parser.spec.raml.RamlTypeExpressionParser
import amf.plugins.document.webapi.parser.spec.{OasDefinitions, toJsonSchema, toOas}
import amf.plugins.domain.shapes.models.ExampleTracking._
import amf.plugins.domain.shapes.models.{FileShape, NodeShape}
import amf.plugins.domain.webapi.annotations.{InvalidBinding, ParameterBindingInBodyLexicalInfo}
import amf.plugins.domain.webapi.metamodel.{ParameterModel, PayloadModel}
import amf.plugins.domain.webapi.models.{Parameter, Payload}
import amf.plugins.features.validation.ParserSideValidations._
import org.yaml.model.{YMap, YMapEntry, YScalar, YType, _}
import scala.collection.mutable

/**
  *
  */
case class RamlParametersParser(map: YMap, adopted: Parameter => Unit, parseOptional: Boolean = false)(
    implicit ctx: RamlWebApiContext) {

  def parse(): Seq[Parameter] =
    map.entries
      .map(entry => ctx.factory.parameterParser(entry, adopted, parseOptional).parse())
}

object RamlHeaderParser {
  def parse(adopted: Parameter => Unit, parseOptional: Boolean = false)(node: YNode)(
      implicit ctx: RamlWebApiContext): Parameter = {
    RamlParameterParser.parse(adopted, parseOptional)(node).withBinding("header")
  }
}

object RamlQueryParameterParser {
  def parse(adopted: Parameter => Unit, parseOptional: Boolean = false)(node: YNode)(
      implicit ctx: RamlWebApiContext): Parameter = {
    RamlParameterParser.parse(adopted, parseOptional)(node).withBinding("query")
  }
}

object RamlParameterParser {
  def parse(adopted: Parameter => Unit, parseOptional: Boolean = false)(node: YNode)(
      implicit ctx: RamlWebApiContext): Parameter = {
    val head = node.as[YMap].entries.head
    ctx.factory.parameterParser(head, adopted, parseOptional).parse()
  }
}

case class Raml10ParameterParser(entry: YMapEntry, adopted: Parameter => Unit, parseOptional: Boolean = false)(
    implicit ctx: RamlWebApiContext)
    extends RamlParameterParser(entry, adopted) {
  override def parse(): Parameter = {

    val name      = ScalarNode(entry.key)
    val parameter = Parameter(entry).set(ParameterModel.Name, name.text()).withParameterName(name.text().toString) // TODO parameter id is using a name that is not final.
    adopted(parameter)

    val p = entry.value.to[YMap] match {
      case Right(map) =>
        map.key("required", (ParameterModel.Required in parameter).explicit.allowingAnnotations)
        map.key("description", (ParameterModel.Description in parameter).allowingAnnotations)
        map.key("binding".asRamlAnnotation, (ParameterModel.Binding in parameter).explicit)

        Raml10TypeParser(entry, shape => shape.withName("schema").adopted(parameter.id))
          .parse()
          .foreach(s => parameter.set(ParameterModel.Schema, tracking(s, parameter.id), Annotations(entry)))

        AnnotationParser(parameter, map).parse()

        parameter
      case _ =>
        val scope = ctx.link(entry.value) match {
          case Left(_) => SearchScope.Fragments
          case _       => SearchScope.Named
        }
        entry.value.tagType match {
          case YType.Null =>
            Raml10TypeParser(
              entry,
              shape => shape.withName("schema").adopted(parameter.id)
            ).parse().foreach { schema =>
              tracking(schema, parameter.id).annotations += SynthesizedField()
              parameter.set(ParameterModel.Schema, schema, Annotations(entry))
            }
            parameter
          case _ => // we have a property type
            entry.value.to[YScalar] match {
              case Right(ref) if ctx.declarations.findParameter(ref.text, scope).isDefined =>
                ctx.declarations
                  .findParameter(ref.text, scope)
                  .get
                  .link(ref.text, Annotations(entry))
                  .asInstanceOf[Parameter]
                  .set(ParameterModel.Name, name.text())
              case Right(ref) if ctx.declarations.findType(ref.text, scope).isDefined =>
                val schema = ctx.declarations
                  .findType(ref.text,
                            scope,
                            Some((s: String) => ctx.violation(InvalidFragmentType, parameter.id, s, entry.value)))
                  .get
                  .link[Shape](ref.text, Annotations(entry))
                  .withName("schema")
                  .adopted(parameter.id)
                parameter.withSchema(schema)
              case Right(ref) if wellKnownType(ref.text, isRef = true) =>
                val schema = parseWellKnownTypeRef(ref.text).withName("schema").adopted(parameter.id)
                parameter.withSchema(schema)

              case Right(ref) if isTypeExpression(ref.text) =>
                RamlTypeExpressionParser(shape => shape.withName("schema").adopted(parameter.id))
                  .parse(ref.text) match {
                  case Some(schema) => parameter.withSchema(schema)
                  case _ =>
                    ctx.violation(UnresolvedParameter,
                                  parameter.id,
                                  s"Cannot parse type expression for unresolved parameter '${parameter.name}'",
                                  entry.value)
                    parameter
                }
              case _ =>
                ctx.violation(UnresolvedParameter, parameter.id, "Cannot declare unresolved parameter", entry.value)
                parameter

            }
        }
    }

    if (p.fields.entry(ParameterModel.Required).isEmpty) {
      val stringName = name.text().toString
      val required   = !stringName.endsWith("?")
      val paramName  = if (required) stringName else stringName.stripSuffix("?")
      p.set(ParameterModel.Required, required)
      p.set(ParameterModel.Name, AmfScalar(paramName, name.text().annotations))
        .set(ParameterModel.ParameterName, paramName)
    }

    p
  }
}

case class Raml08ParameterParser(entry: YMapEntry, adopted: Parameter => Unit, parseOptional: Boolean = false)(
    implicit ctx: RamlWebApiContext)
    extends RamlParameterParser(entry, adopted) {
  def parse(): Parameter = {

    val name      = ScalarNode(entry.key)
    val parameter = Parameter(entry).set(ParameterModel.Name, name.text()).withParameterName(name.text().toString)
    adopted(parameter)

    entry.value.tagType match {
      case YType.Null =>
        Raml10TypeParser(
          entry,
          shape => shape.withName("schema").adopted(parameter.id)
        ).parse().foreach { schema =>
          tracking(schema, parameter.id).annotations += SynthesizedField()
          parameter.set(ParameterModel.Schema, schema, Annotations(entry))
        }
      case _ =>
        // Named Parameter Parse
        Raml08TypeParser(entry,
                         (s: Shape) => s.withName(name.text().toString).adopted(parameter.id),
                         isAnnotation = false,
                         StringDefaultType)
          .parse()
          .foreach(s => parameter.withSchema(tracking(s, parameter.id)))
    }

    entry.value.toOption[YMap] match {
      case Some(map) =>
        map.key("required", (ParameterModel.Required in parameter).explicit)
      case _ =>
    }

    if (parameter.fields.entry(ParameterModel.Required).isEmpty) parameter.set(ParameterModel.Required, value = false)

    val stringName = name.text().toString
    if (parseOptional && stringName.endsWith("?")) {
      parameter.set(ParameterModel.Optional, value = true)
      val n = stringName.stripSuffix("?")
      parameter.set(ParameterModel.Name, AmfScalar(n, name.text().annotations)).set(ParameterModel.ParameterName, n)
    }

    parameter
  }
}

abstract class RamlParameterParser(entry: YMapEntry, adopted: Parameter => Unit)(implicit val ctx: RamlWebApiContext)
    extends RamlTypeSyntax
    with SpecParserOps {
  def parse(): Parameter
}

case class OasParameterParser(entryOrNode: Either[YMapEntry, YNode], parentId: String, nameNode: Option[YNode])(
    implicit ctx: WebApiContext)
    extends SpecParserOps {

  private val map = entryOrNode match {
    case Left(entry) => entry.value.as[YMap]
    case Right(node) => node.as[YMap]
  }

  private def setName(p: DomainElement with NamedDomainElement): DomainElement = {
    p match {
      case _: Shape if nameNode.isDefined =>
        p.set(ShapeModel.Name, nameNode.map(ScalarNode(_).text()).get)
      case _: Shape =>
        map.key("name", (ShapeModel.Name in p).withAnnotation(Inferred())) // name of the parameter in the HTTP binding (path, request parameter, etc)
      case p: Payload =>
        if (nameNode.nonEmpty)
          p.set(
            ParameterModel.Name,
            nameNode.map(ScalarNode(_).text()).get,
            map
              .key("name")
              .map(e => {
                Annotations(ParameterNameForPayload(ScalarNode(e.value).text().value.toString, Range(e.range)))
              })
              .getOrElse(Annotations())
          )
        else map.key("name", ParameterModel.Name in p)
        validateEntryName(p)
      case _ =>
        if (nameNode.nonEmpty)
          p.set(ParameterModel.Name, nameNode.map(ScalarNode(_).text()).get)
        else
          map.key("name", ParameterModel.Name in p) // name of the parameter in the HTTP binding (path, request parameter, etc)
        validateEntryName(p)
    }
    p
  }

  private def buildFromBinding(in: String, bindingEntry: Option[YMapEntry]): OasParameter = {
    in match {
      case "body" =>
        OasParameter(parseBodyPayload(bindingEntry.map(a => Range(a.range))),
                     entryOrNode.toOption.orElse(entryOrNode.left.toOption))
      case "formData" =>
        OasParameter(parseFormDataPayload(bindingEntry.map(a => Range(a.range))),
                     entryOrNode.toOption.orElse(entryOrNode.left.toOption))
      case "query" | "header" | "path" =>
        OasParameter(parseCommonParam(), entryOrNode.toOption.orElse(entryOrNode.left.toOption))
      case _ =>
        val oasParam = buildFromBinding(defaultBinding, None)
        invalidBinding(bindingEntry, in, oasParam)
        oasParam
    }
  }

  private def validateEntryName(element: DomainElement with NamedDomainElement): Unit = {
    if (element.name.option().isEmpty) element.withName("default")
    element.adopted(parentId)
    if (map.key("name").isEmpty) {
      ctx.violation(
        ParameterNameRequired,
        element.id,
        "'name' property is required in a parameter.",
        map
      )
    }
  }

  def parse(): OasParameter = {
    map.key("$ref") match {
      case Some(ref) => parseParameterRef(ref, parentId)
      case None =>
        map.key("in") match {
          case Some(entry: YMapEntry) =>
            val in = entry.value.as[YScalar].text
            buildFromBinding(in, Some(entry))
          case _ => // ignore
            /**
              * Binding is required, i'm not setting any default value so It will be some model validation.
              * */
            val parameter = Parameter(map)
            setName(parameter)
            parameter.adopted(parentId)
            OasParameter(parameter)
        }
    }
  }

  private def parseCommonParam(): Parameter = {
    val parameter = Parameter(entryOrNode.toOption.getOrElse(entryOrNode.left.get))
    setName(parameter)
    parameter.adopted(parentId)
    parameter.set(ParameterModel.Required, value = false)

    map.key("name", ParameterModel.ParameterName in parameter) // name of the parameter in the HTTP binding (path, request parameter, etc)

    map.key("in", ParameterModel.Binding in parameter)
    map.key("description", ParameterModel.Description in parameter)
    map.key("required", (ParameterModel.Required in parameter).explicit)

    ctx.closedShape(parameter.id, map, "parameter")
    OasTypeParser(
      entryOrNode,
      "schema",
      map,
      shape => shape.withName("schema").adopted(parameter.id),
      OAS20SchemaVersion(position = "parameter")
    )(toOas(ctx))
      .parse()
      .map { schema =>
        parameter.set(ParameterModel.Schema, schema, Annotations(map))
      }
      .orElse {
        ctx.violation(
          UnresolvedParameter,
          parameter.id,
          "Cannot find valid schema for parameter",
          map
        )
        None
      }
    AnnotationParser(parameter, map).parse()
    parameter
  }

  private def parseFormDataPayload(bindingRange: Option[Range]) = {
    val payload = commonPayload(bindingRange)
    ctx.closedShape(payload.id, map, "parameter")
    OasTypeParser(
      entryOrNode,
      "schema",
      map,
      shape => setName(shape).asInstanceOf[Shape].adopted(payload.id),
      OAS20SchemaVersion(position = "parameter")
    )(toOas(ctx))
      .parse()
      .map { schema =>
        payload.set(PayloadModel.Schema, tracking(schema, payload.id), Annotations(map))
      }
      .orElse {
        ctx.violation(
          UnresolvedParameter,
          payload.id,
          "Cannot find valid schema for parameter",
          map
        )
        None
      }
    payload.annotations += FormBodyParameter()
    payload
  }

  private def commonPayload(bindingRange: Option[Range]): Payload = {
    val payload = Payload(entryOrNode.toOption.getOrElse(entryOrNode.left.get))
    setName(payload)
    if (payload.name.option().isEmpty)
      payload.set(ParameterModel.Name, AmfScalar("default"), Annotations() += Inferred())
    map.key("required", entry => {
      val req: Boolean = entry.value.as[Boolean]
      payload.annotations += RequiredParamPayload(req, Range(entry.range))
    })

    AnnotationParser(payload, map).parse()
    payload
  }

  private def parseBodyPayload(bindingRange: Option[Range]): Payload = {
    val payload: Payload = commonPayload(bindingRange)
    ctx.closedShape(payload.id, map, "bodyParameter")

    map.key(
      "schema",
      entry => {
        OasTypeParser(entry, shape => setName(shape).asInstanceOf[Shape].adopted(payload.id))(toOas(ctx)) // i don't need to set param need in here. Its necesary only for form data, because of the properties
          .parse()
          .map { schema =>
            checkNotFileInBody(schema)
            payload.set(PayloadModel.Schema, tracking(schema, payload.id), Annotations(entry))
            bindingRange.foreach { range =>
              schema.annotations += ParameterBindingInBodyLexicalInfo(range)
            }
            schema
          }
      }
    )

    map.key("mediaType".asOasExtension, PayloadModel.MediaType in payload)
    payload
  }

  private def invalidBinding(bindingEntry: Option[YMapEntry], binding: String, parameter: OasParameter): Unit = {
    val entryValueAnnotations = bindingEntry.map(e => Annotations(e.value)).getOrElse(Annotations())

    ctx.violation(OasInvalidParameterBinding,
                  "",
                  s"Invalid parameter binding '$binding'",
                  bindingEntry.map(_.value).getOrElse(map))

    parameter.domainElement match {
      case p: Parameter =>
        p.set(ParameterModel.Binding,
              AmfScalar(p.binding.value(), entryValueAnnotations),
              entryValueAnnotations += InvalidBinding(binding))
      case p: Payload =>
        p.add(InvalidBinding(binding))
      case _ => // ignore
    }
  }

  def defaultBinding: String = map.key("schema").map(_ => "body").getOrElse("query")

  protected def checkNotFileInBody(schema: Shape): Unit = {
    val schemaToCheck =
      if (schema.isLink) schema.linkTarget
      else schema
    if (schemaToCheck.isInstanceOf[FileShape])
      ctx.violation(
        OasFormDataNotFileSpecification,
        schema.id,
        "File types in parameters must be declared in formData params",
        map
      )
  }

  protected def parseParameterRef(ref: YMapEntry, parentId: String): OasParameter = {
    val refUrl = OasDefinitions.stripParameterDefinitionsPrefix(ref.value)
    ctx.declarations.findParameter(refUrl, SearchScope.All) match {
      case Some(param) =>
        val parameter: Parameter = param.link(refUrl, Annotations(map))
        parameter.withName(refUrl).adopted(parentId)
        OasParameter(parameter, Some(ref))
      case None =>
        ctx.declarations.findPayload(refUrl, SearchScope.All) match {
          case Some(payload) =>
            OasParameter(payload.link(refUrl, Annotations(map)).asInstanceOf[Payload], Some(ref))
          case None =>
            val fullRef = ctx.resolvedPath(ctx.rootContextDocument, refUrl)
            ctx.parseRemoteOasParameter(fullRef, parentId)(toJsonSchema(ctx)) match {
              case Some(oasParameter) => oasParameter
              case _ =>
                val parameter = Parameter()
                setName(parameter)
                parameter.adopted(parentId)
                ctx.violation(UnresolvedParameter,
                              parameter.id,
                              s"Cannot find parameter or payload reference $refUrl",
                              ref)
                OasParameter(parameter, Some(ref))
            }
        }
    }
  }
}

case class OasParametersParser(values: Seq[YNode], parentId: String)(implicit ctx: OasWebApiContext) {

  def formDataPayload(formData: Seq[Payload]): Option[Payload] =
    if (formData.isEmpty) None
    else {
      val schema = NodeShape().withName("formData").adopted(parentId)

      formData.foreach { p =>
        val payload = if (p.isLink) p.effectiveLinkTarget().asInstanceOf[Payload] else p

        val property = schema.withProperty(payload.name.value())
        payload.annotations.find(classOf[RequiredParamPayload]) match {
          case None => property.set(PropertyShapeModel.MinCount, 0)
          case Some(a) if !a.required =>
            property.set(PropertyShapeModel.MinCount,
                         AmfScalar(0, Annotations() += LexicalInformation(a.range) += ExplicitField()))
          case Some(a) =>
            property.set(PropertyShapeModel.MinCount,
                         AmfScalar(1, Annotations() += LexicalInformation(a.range) += ExplicitField()))
        }

        Option(payload.schema).foreach(property.withRange(_).adopted(property.id))
      }

      Some(Payload().withName("formData").adopted(parentId).set(PayloadModel.Schema, schema).add(FormBodyParameter()))
    }

  def parse(inRequest: Boolean = false): Parameters = {
    val oasParameters = values
      .map(value => OasParameterParser(Right(value), parentId, None).parse())

    val formData = oasParameters.flatMap(_.formData)
    val body     = oasParameters.filter(_.isBody)

    val parameters       = oasParameters.flatMap(_.parameter)
    val nameWithBindings = parameters.map(param => param.parameterName.value() -> param.binding.value())
    obtainDuplicated(nameWithBindings.toList).foreach {
      case (name, binding) =>
        oasParameters
          .find(oasParam =>
            oasParam.parameter.exists(param =>
              param.parameterName.value() == name && param.binding.value() == binding))
          .foreach(
            oasParam =>
              ctx.violation(
                DuplicatedParameters,
                oasParam.domainElement.id,
                s"parameter $name of type $binding was found duplicated",
                oasParam.ast.get
            ))
    }

    if (inRequest) {
      if (body.nonEmpty && formData.nonEmpty) {
        val bodyParam = body.head
        ctx.violation(
          OasBodyAndFormDataParameterSpecification,
          bodyParam.domainElement.id,
          "Cannot declare body and formData params at the same time for a request",
          bodyParam.ast.get
        )
      }

      validateOasPayloads(body, OasInvalidBodyParameter)
    }

    Parameters(
      oasParameters.flatMap(_.query) ++ oasParameters.flatMap(_.invalids),
      oasParameters.flatMap(_.path),
      oasParameters.flatMap(_.header),
      Nil,
      body.flatMap(_.body) ++ formDataPayload(formData)
    )
  }

  @annotation.tailrec
  private def obtainDuplicated[A](list: List[A], seen: mutable.Set[A] = mutable.HashSet[A]()): Option[A] =
    list match {
      case x :: xs => if (seen.contains(x)) Some(x) else obtainDuplicated(xs, seen += x)
      case _       => None
    }

  private def validateOasPayloads(params: Seq[OasParameter], id: ValidationSpecification): Unit =
    if (params.length > 1) {
      params.tail.foreach { param =>
        ctx.violation(
          id,
          param.domainElement.id,
          "Cannot declare more than one body parameter for a request",
          param.ast.get
        )
      }
    }
}
