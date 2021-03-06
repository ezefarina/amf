package amf.plugins.domain.webapi.metamodel

import amf.core.metamodel.Field
import amf.core.metamodel.Type.{Array, Str}
import amf.core.metamodel.domain.{DomainElementModel, ModelDoc, ModelVocabularies}
import amf.core.metamodel.domain.common.NameFieldSchema
import amf.core.metamodel.domain.templates.KeyField
import amf.core.vocabulary.Namespace.{Http, Schema}
import amf.core.vocabulary.ValueType
import amf.plugins.domain.webapi.models.Callback

/**
  * Callback metaModel.
  */
object CallbackModel extends DomainElementModel with KeyField with NameFieldSchema {

  val Expression = Field(
    Str,
    Http + "expression",
    ModelDoc(ModelVocabularies.Http, "expression", "structural location of the information to fulfill the callback"))

  val Endpoint = Field(EndPointModel,
                       Http + "endpoint",
                       ModelDoc(ModelVocabularies.Http, "endpoint", "Endpoint targeted by the callback"))

  override val `type`: List[ValueType] = Http + "Callback" :: DomainElementModel.`type`

  override def fields: List[Field] = Name :: Expression :: Endpoint :: DomainElementModel.fields

  override def modelInstance = Callback()

  override val key: Field = Name

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.Http,
    "Callback",
    "Model defining the information for a HTTP callback/ webhook"
  )
}
