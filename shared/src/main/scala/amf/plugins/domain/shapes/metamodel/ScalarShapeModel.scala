package amf.plugins.domain.shapes.metamodel

import amf.core.metamodel.Field
import amf.core.metamodel.Type.Iri
import amf.core.metamodel.domain.DomainElementModel
import amf.plugins.domain.shapes.models.ScalarShape
import amf.core.vocabulary.Namespace.Shacl
import amf.core.vocabulary.ValueType

/**
  * Scalar shape metamodel
  */
object ScalarShapeModel extends ShapeModel with DomainElementModel with CommonShapeFields {

  val DataType = Field(Iri, Shacl + "datatype")

  override val fields
    : List[Field] = List(DataType) ++ commonOASFields ++ ShapeModel.fields ++ DomainElementModel.fields

  override val `type`: List[ValueType] = List(Shacl + "ScalarShape") ++ ShapeModel.`type` ++ DomainElementModel.`type`

  override def modelInstance = ScalarShape()
}
