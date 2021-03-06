package amf.plugins.domain.webapi.resolution.stages

import amf.core.metamodel.Field
import amf.core.model.document.{BaseUnit, Document}
import amf.core.model.domain.{AmfScalar, DomainElement}
import amf.core.parser.ErrorHandler
import amf.core.resolution.stages.ResolutionStage
import amf.plugins.domain.webapi.metamodel._
import amf.plugins.domain.webapi.models.{Payload, WebApi}
import amf.{OasProfile, ProfileName}
import amf.plugins.domain.shapes.models.ExampleTracking.tracking

/** Apply root and operation mime types to payloads.
  *
  * Request payloads will have as default mime type the 'accepts' field.
  * Response payloads will have as default mime type the 'contentType' field.
  */
class MediaTypeResolutionStage(profile: ProfileName, isValidation: Boolean = false)(
    override implicit val errorHandler: ErrorHandler)
    extends ResolutionStage() {
  override def resolve[T <: BaseUnit](model: T): T = {
    model match {
      case doc: Document if doc.encodes.isInstanceOf[WebApi] =>
        resolvePayloads(doc.encodes.asInstanceOf[WebApi])
        resolveMediaTypes(doc.encodes.asInstanceOf[WebApi])
      case _ =>
    }
    model.asInstanceOf[T]
  }

  def resolvePayloads(api: WebApi): Unit = {
    api.endPoints.foreach { endpoint =>
      val payloads = endpoint.payloads
      endpoint.fields.removeField(EndPointModel.Payloads)
      if (payloads.nonEmpty) {
        endpoint.operations.foreach { operation =>
          Option(operation.request) match {
            case Some(request) =>
              payloads.foreach { payload =>
                request.add(RequestModel.Payloads, payload)
              }
            case None => operation.withRequest().withPayloads(payloads)
          }
        }
      }
    }
  }

  def resolveMediaTypes(api: WebApi): Unit = {
    val rootAccepts     = field(api, WebApiModel.Accepts)
    val rootContentType = field(api, WebApiModel.ContentType)

    api.endPoints.foreach { endPoint =>
      endPoint.operations.foreach { operation =>
        // I need to know if this is an empty array or if it's not defined.
        val opAccepts     = field(operation, OperationModel.Accepts)
        val opContentType = field(operation, OperationModel.ContentType)

        val accepts     = merge(rootAccepts, opAccepts)
        val contentType = merge(rootContentType, opContentType)

        Option(operation.request).foreach { request =>
          // Use accepts field.
          accepts match {
            case Some(a) =>
              if (!isValidation && profile == OasProfile) operation.set(OperationModel.Accepts, a)
              request.setArray(RequestModel.Payloads, payloads(request.payloads, a, request.id))
            case None =>
          }
        }

        operation.responses.foreach { response =>
          // Use contentType field.
          contentType match {
            case Some(ct) =>
              if (!isValidation && profile == OasProfile) operation.set(OperationModel.ContentType, ct)
              response.setArray(RequestModel.Payloads, payloads(response.payloads, ct, response.id))
            case None =>
          }
        }
      }
    }
  }

  /** Get and remove field from domain element */
  private def field(element: DomainElement, field: Field) = {
    val result = element.fields.entry(field).map(_.array.values.map(v => v.asInstanceOf[AmfScalar].toString))
    if (!isValidation) element.fields.removeField(field)
    result
  }

  private def payloads(p: Seq[Payload], mediaTypes: Seq[String], parent: String) = {
    var (noMediaType, result) = p.partition(_.fields.entry(PayloadModel.MediaType).isEmpty)

    noMediaType.foreach { payload =>
      mediaTypes.foreach { mediaType =>
        // Schema must not be empty, or it would be an empty payload ¯\_(ツ)_/¯
        result = result :+ {
          val parsedPayload = Payload(payload.annotations)
            .withMediaType(mediaType)
            .adopted(parent)
          if (Option(payload.schema).isDefined)
            parsedPayload.fields
              .setWithoutId(PayloadModel.Schema, tracking(payload.schema, parsedPayload.id, Some(payload.id)))
          parsedPayload
        }
      }
    }

    result
  }

  def merge(root: Option[Seq[String]], op: Option[Seq[String]]): Option[Seq[String]] =
    op.orElse(root).filter(_.nonEmpty)
}
