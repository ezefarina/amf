package amf.plugins.domain.webapi.metamodel.templates

import amf.core.metamodel.domain.{ModelDoc, ModelVocabularies}
import amf.core.metamodel.domain.templates.ParametrizedDeclarationModel
import amf.plugins.domain.webapi.models.templates.ParametrizedTrait
import amf.core.vocabulary.Namespace.Document
import amf.core.vocabulary.ValueType

object ParametrizedTraitModel extends ParametrizedDeclarationModel {
  override val `type`: List[ValueType] = Document + "ParametrizedTrait" :: ParametrizedDeclarationModel.`type`

  override def modelInstance = ParametrizedTrait()

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.AmlDoc,
    "Parametrized Trait",
    "RAML trait with declared parameters"
  )
}
