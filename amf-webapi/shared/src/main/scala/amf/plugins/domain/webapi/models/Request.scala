package amf.plugins.domain.webapi.models

import amf.core.metamodel.Obj
import amf.core.model.{BoolField, StrField}
import amf.core.model.domain.{DomainElement, Shape}
import amf.core.parser.{Annotations, Fields}
import amf.plugins.domain.webapi.metamodel.RequestModel
import amf.plugins.domain.webapi.metamodel.RequestModel._

/**
  * Request internal model.
  */
case class Request(fields: Fields, annotations: Annotations) extends DomainElement {

  def description: StrField            = fields.field(Description)
  def required: BoolField              = fields.field(Required)
  def queryParameters: Seq[Parameter]  = fields.field(QueryParameters)
  def headers: Seq[Parameter]          = fields.field(Headers)
  def payloads: Seq[Payload]           = fields.field(Payloads)
  def queryString: Shape               = fields.field(QueryString)
  def uriParameters: Seq[Parameter]    = fields.field(UriParameters)
  def cookieParameters: Seq[Parameter] = fields.field(CookieParameters)

  def withDescription(description: String): this.type                   = set(Description, description)
  def withRequired(required: Boolean): this.type                        = set(Required, required)
  def withQueryParameters(parameters: Seq[Parameter]): this.type        = setArray(QueryParameters, parameters)
  def withHeaders(headers: Seq[Parameter]): this.type                   = setArray(Headers, headers)
  def withPayloads(payloads: Seq[Payload]): this.type                   = setArray(Payloads, payloads)
  def withQueryString(queryString: Shape): this.type                    = set(QueryString, queryString)
  def withUriParameters(uriParameters: Seq[Parameter]): this.type       = setArray(UriParameters, uriParameters)
  def withCookieParameters(cookieParameters: Seq[Parameter]): this.type = setArray(CookieParameters, cookieParameters)

  def withQueryParameter(name: String): Parameter = {
    val result = Parameter().withName(name)
    add(QueryParameters, result)
    result
  }

  def withHeader(name: String): Parameter = {
    val result = Parameter().withName(name)
    add(Headers, result)
    result
  }

  def withUriParameter(name: String): Parameter = {
    val result = Parameter().withName(name)
    add(UriParameters, result)
    result
  }

  def withCookieParameter(name: String): Parameter = {
    val result = Parameter().withName(name)
    add(CookieParameters, result)
    result
  }

  def withPayload(mediaType: Option[String] = None): Payload = {
    val result = Payload()
    mediaType.map(result.withMediaType)
    add(Payloads, result)
    result
  }

  def getAllParameters: Seq[DomainElement] =
    queryParameters ++ headers ++ payloads ++ uriParameters ++ cookieParameters

  override def meta: Obj = RequestModel

  /** Value , path + field value that is used to compose the id when the object its adopted */
  override def componentId: String = "/request"
}

object Request {

  def apply(): Request = apply(Annotations())

  def apply(annotations: Annotations): Request = new Request(Fields(), annotations)
}
