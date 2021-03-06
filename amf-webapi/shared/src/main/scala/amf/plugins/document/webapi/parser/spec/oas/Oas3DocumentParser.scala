package amf.plugins.document.webapi.parser.spec.oas

import amf.core.Root
import amf.core.parser._
import amf.core.utils.Strings
import amf.plugins.document.webapi.contexts.OasWebApiContext
import amf.plugins.domain.webapi.metamodel._
import amf.plugins.domain.webapi.models.WebApi
import org.yaml.model._

case class Oas3DocumentParser(root: Root)(implicit override val ctx: OasWebApiContext)
    extends OasDocumentParser(root) {
  override def parseWebApi(map: YMap): WebApi = {
    val api = super.parseWebApi(map)

    map.key("consumes".asOasExtension, WebApiModel.Accepts in api)
    map.key("produces".asOasExtension, WebApiModel.ContentType in api)
    map.key("schemes".asOasExtension, WebApiModel.Schemes in api)

    api
  }
}
