package amf.plugins.document.webapi.parser.spec.declaration

import amf.core.parser.{Annotations, _}
import amf.core.remote.{Oas, Raml}
import amf.plugins.document.webapi.contexts.WebApiContext
import amf.plugins.document.webapi.parser.spec.common.{AnnotationParser, SpecParserOps}
import amf.plugins.domain.shapes.metamodel.CreativeWorkModel
import amf.plugins.domain.shapes.models.CreativeWork
import org.yaml.model.{YMap, YNode}
import amf.core.utils.Strings
import amf.plugins.features.validation.ParserSideValidations.UnexpectedVendor

object OasCreativeWorkParser {
  def parse(node: YNode)(implicit ctx: WebApiContext): CreativeWork = OasCreativeWorkParser(node.as[YMap]).parse()
}

/**
  *
  */
case class OasCreativeWorkParser(node: YNode)(implicit val ctx: WebApiContext) extends SpecParserOps {
  def parse(): CreativeWork = {

    val map          = node.as[YMap]
    val creativeWork = CreativeWork(node)

    map.key("url", CreativeWorkModel.Url in creativeWork)
    map.key("description", CreativeWorkModel.Description in creativeWork)
    map.key("title".asOasExtension, CreativeWorkModel.Title in creativeWork)

    AnnotationParser(creativeWork, map).parse()

    creativeWork
  }
}

case class RamlCreativeWorkParser(node: YNode)(implicit val ctx: WebApiContext) extends SpecParserOps {
  def parse(): CreativeWork = {

    val map           = node.as[YMap]
    val documentation = CreativeWork(Annotations.valueNode(node))

    map.key("title", (CreativeWorkModel.Title in documentation).allowingAnnotations)
    map.key("content", (CreativeWorkModel.Description in documentation).allowingAnnotations)

    val url = ctx.vendor match {
      case _: Oas  => "url"
      case _: Raml => "url".asRamlAnnotation
      case other =>
        ctx.violation(UnexpectedVendor, s"Unexpected vendor '$other'", node)
        "url"
    }

    map.key(url, CreativeWorkModel.Url in documentation)

    AnnotationParser(documentation, map).parse()

    documentation
  }
}
