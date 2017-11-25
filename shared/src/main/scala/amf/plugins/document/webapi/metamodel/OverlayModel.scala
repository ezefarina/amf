package amf.plugins.document.webapi.metamodel

import amf.core.metamodel.document.{DocumentModel, ExtensionLikeModel}
import amf.plugins.document.webapi.model.Overlay
import amf.core.vocabulary.Namespace.Document
import amf.core.vocabulary.ValueType

object OverlayModel extends ExtensionLikeModel {
  override val `type`: List[ValueType] = List(Document + "Overlay") ++ DocumentModel.`type`

  override def modelInstance = Overlay()
}
