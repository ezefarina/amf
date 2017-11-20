package amf.spec.domain

import amf.domain.Annotation.SynthesizedField
import amf.domain.{Annotations, Payload}
import amf.plugins.domain.webapi.contexts.WebApiContext
import amf.spec.ParserContext
import amf.spec.common.{AnnotationParser, ValueNode}
import amf.spec.declaration.{AnyDefaultType, RamlTypeParser}
import org.yaml.model.{YMap, YMapEntry, YType}

/**
  *
  */
case class RamlPayloadParser(entry: YMapEntry, producer: (Option[String]) => Payload)(implicit ctx: WebApiContext) {
  def parse(): Payload = {

    val payload = producer(Some(ValueNode(entry.key).string().value.toString)).add(Annotations(entry))

    entry.value.to[YMap] match {
      case Right(map) =>
        // TODO
        // Should we clean the annotations here so they are not parsed again in the shape?
        AnnotationParser(() => payload, map).parse()
      case _ =>
    }

    entry.value.tagType match {
      case YType.Null =>
        RamlTypeParser(entry, shape => shape.withName("schema").adopted(payload.id), isAnnotation = false, AnyDefaultType)
          .parse()
          .foreach { schema =>
            schema.annotations += SynthesizedField()
            payload.withSchema(schema)
          }
      case _ =>
        RamlTypeParser(entry, shape => shape.withName("schema").adopted(payload.id), isAnnotation = false, AnyDefaultType)
          .parse()
          .foreach(payload.withSchema)

    }
    payload
  }
}
