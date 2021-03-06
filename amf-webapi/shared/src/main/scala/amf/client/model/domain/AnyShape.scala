package amf.client.model.domain

import amf.client.convert.WebApiClientConverters._
import amf.client.environment.Environment
import amf.client.model.document.PayloadFragment
import amf.client.validate.{PayloadValidator, ValidationReport}
import amf.plugins.domain.shapes.models.{AnyShape => InternalAnyShape}

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
class AnyShape(override private[amf] val _internal: InternalAnyShape) extends Shape {

  @JSExportTopLevel("model.domain.AnyShape")
  def this() = this(InternalAnyShape())

  def documentation: CreativeWork     = _internal.documentation
  def xmlSerialization: XMLSerializer = _internal.xmlSerialization
  def examples: ClientList[Example]   = _internal.examples.asClient

  def withDocumentation(documentation: CreativeWork): this.type = {
    _internal.withDocumentation(documentation)
    this
  }

  def withXMLSerialization(xmlSerialization: XMLSerializer): this.type = {
    _internal.withXMLSerialization(xmlSerialization)
    this
  }

  def withExamples(examples: ClientList[Example]): this.type = {
    _internal.withExamples(examples.asInternal)
    this
  }

  def withExample(mediaType: String): Example = _internal.withExample(Some(mediaType))

  override def linkCopy(): AnyShape = _internal.linkCopy()
//  def build(shape: InternalAnyShape): Shape = platform.wrap[Shape](shape) ???

  /** if the shape was parsed of a json schema, or has been previously generated a new json schema, returns thar value,
    otherwise generate a new json schema and store the value for futures invocations */
  def toJsonSchema: String = _internal.toJsonSchema

  /** Force a new json schema generation, no matter if the shape was parsed from that kind of expression or if was previously generated.
    * Stores the result for futures toJsonSchema invocations.
    * Should use this method when you have mutated this instance */
  def buildJsonSchema(): String = _internal.buildJsonSchema()

  /** If the shape was parsed of RAML, or a new RAML has been previously generated,
    * returns that value, otherwise generates a new RAML Data Type and stores
    * the value for futures invocations.
    * Proxies call to internal AnyShape.toRamlDatatype.
    */
  def toRamlDatatype: String = _internal.toRamlDatatype

  /** Forces a new RAML Data Type generation, no matter if the shape was
    * parsed from that kind of expression or if was previously generated.
    * Stores the result for futures toRamlDatatype invocations.
    * Call this method to generate a new RAML Data Type when this
    * instance was mutated.
    * Proxies call to internal AnyShape.toRamlDatatype.
    */
  def buildRamlDatatype(): String = _internal.buildRamlDatatype

  def validate(payload: String, env: Environment): ClientFuture[ValidationReport] =
    _internal.validate(payload, env._internal).asClient

  def validate(payload: String): ClientFuture[ValidationReport] = _internal.validate(payload).asClient

  def validate(fragment: PayloadFragment, env: Environment): ClientFuture[ValidationReport] =
    _internal.validate(fragment._internal, env._internal).asClient

  def validate(fragment: PayloadFragment): ClientFuture[ValidationReport] =
    _internal.validate(fragment._internal).asClient

  def validateParameter(payload: String, env: Environment): ClientFuture[ValidationReport] =
    _internal.validateParameter(payload, env._internal).asClient

  def validateParameter(payload: String): ClientFuture[ValidationReport] =
    _internal.validateParameter(payload).asClient

  def payloadValidator(mediaType: String): ClientOption[PayloadValidator] =
    _internal.payloadValidator(mediaType).asClient

  def parameterValidator(mediaType: String): ClientOption[PayloadValidator] =
    _internal.parameterValidator(mediaType).asClient

  def payloadValidator(mediaType: String, env: Environment): ClientOption[PayloadValidator] =
    _internal.payloadValidator(mediaType, env._internal).asClient

  def parameterValidator(mediaType: String, env: Environment): ClientOption[PayloadValidator] =
    _internal.parameterValidator(mediaType, env._internal).asClient

  /** Aux method to know when the shape is instance only of any shape
    * and it's because was parsed from
    * an empty (or only with example) payload, an not an explicit type def */
  def isDefaultEmpty: Boolean = _internal.isDefaultEmpty

  /**
    * @param trackId parent id of the original payload type with simpleinheritance and the searched example.
    * @return A ClientOption of the original inlined example, or empty if there is not any example with
    *         that track information annotated.
    */
  def trackedExample(trackId: String): ClientOption[Example] = _internal.trackedExample(trackId).asClient
}
