package amf.plugins.document.webapi.annotations

import amf.core.model.domain.{AmfElement, AnnotationGraphLoader, SerializableAnnotation}

object ParsedFromTypeExpression extends AnnotationGraphLoader {
  override def unparse(annotatedValue: String, objects: Map[String, AmfElement]) = {
    ParsedFromTypeExpression(annotatedValue)
  }
}

case class ParsedFromTypeExpression(expression: String) extends SerializableAnnotation {
  override val name: String  = "type-exprssion"
  override val value: String = expression
}