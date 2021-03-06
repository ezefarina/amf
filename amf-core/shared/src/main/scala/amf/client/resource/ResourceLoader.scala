package amf.client.resource

import amf.client.convert.CoreClientConverters._
import amf.client.remote.Content

import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
trait ResourceLoader {

  /** Fetch specified resource and return associated content. Resource should have benn previously accepted. */
  /** If the resource not exists, you should return a future failed with an [[ResourceNotFound]] exception. */
  def fetch(resource: String): ClientFuture[Content]

  /** Accepts specified resource. */
  def accepts(resource: String): Boolean = true
}