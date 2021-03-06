package amf.plugins.document.graph

import amf.client.plugins.{AMFDocumentPlugin, AMFPlugin}
import amf.core.Root
import amf.core.client.ParsingOptions
import amf.core.emitter.RenderOptions
import amf.core.metamodel.Obj
import amf.core.model.document.BaseUnit
import amf.core.model.domain.AnnotationGraphLoader
import amf.core.parser._
import amf.core.rdf.{RdfModelDocument, RdfModelParser}
import amf.core.remote.{Amf, Platform}
import amf.core.resolution.pipelines.{BasicResolutionPipeline, ResolutionPipeline}
import amf.core.unsafe.PlatformSecrets
import amf.core.vocabulary.Namespace
import amf.plugins.document.graph.parser.{GraphDependenciesReferenceHandler, GraphParser, JsonLdEmitter}
import org.yaml.builder.DocBuilder
import org.yaml.model.{YDocument, YMap, YSequence, YScalar}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AMFGraphPlugin extends AMFDocumentPlugin with PlatformSecrets {

  override def init(): Future[AMFPlugin] = Future { this }

  override val ID: String     = Amf.name
  override def dependencies() = Seq()

  val vendors = Seq(Amf.name)

  override def modelEntities: Seq[Obj] = Nil

  override def serializableAnnotations(): Map[String, AnnotationGraphLoader] = Map.empty

  override def documentSyntaxes = Seq(
    "application/ld+json",
    "application/json",
    "application/amf+json"
  )

  override def canParse(root: Root): Boolean = {
    root.parsed match {
      case parsed: SyamlParsedDocument =>
        val maybeMaps = parsed.document.node.toOption[Seq[YMap]]
        val maybeMap  = maybeMaps.flatMap(s => s.headOption)
        maybeMap match {
          case Some(m: YMap) =>
            val toDocumentNamespace: String => String = a => (Namespace.Document + a).iri()

            val acceptedKeys  = Seq("encodes", "declares", "references").map(toDocumentNamespace)
            val acceptedTypes = Seq("Document", "Fragment", "Module", "Unit").map(toDocumentNamespace)

            acceptedKeys.exists(m.key(_).isDefined) ||
            m.key("@type").exists { typesEntry =>
              val retrievedTypes = typesEntry.value.as[YSequence].nodes.map(node => node.as[YScalar].value)
              (acceptedTypes intersect retrievedTypes).nonEmpty
            }
          case _ => false
        }
      case _: RdfModelDocument => true

      case _ => false
    }
  }

  override def parse(root: Root, ctx: ParserContext, platform: Platform, options: ParsingOptions): Option[BaseUnit] =
    root.parsed match {
      case parsed: SyamlParsedDocument =>
        Some(GraphParser(platform).parse(parsed.document, effectiveUnitUrl(root.location, options)))
      case parsed: RdfModelDocument =>
        Some(new RdfModelParser(platform)(ctx).parse(parsed.model, effectiveUnitUrl(root.location, options)))
      case _ =>
        None
    }

  override def canUnparse(unit: BaseUnit) = true

  override def emit[T](unit: BaseUnit, builder: DocBuilder[T], renderOptions: RenderOptions): Boolean =
    JsonLdEmitter.emit(unit, builder, renderOptions)

  override protected def unparseAsYDocument(unit: BaseUnit, renderOptions: RenderOptions): Option[YDocument] =
    throw new IllegalStateException("Unreachable")

  override def referenceHandler(eh: ErrorHandler): ReferenceHandler = GraphDependenciesReferenceHandler

  /**
    * Resolves the provided base unit model, according to the semantics of the domain of the document
    */
  override def resolve(unit: BaseUnit,
                       errorHandler: ErrorHandler,
                       pipelineId: String = ResolutionPipeline.DEFAULT_PIPELINE): BaseUnit =
    new BasicResolutionPipeline(errorHandler).resolve(unit)

  /**
    * Does references in this type of documents be recursive?
    */
  override val allowRecursiveReferences: Boolean = true

  protected def effectiveUnitUrl(location: String, options: ParsingOptions): String = {
    options.definedBaseUrl match {
      case Some(url) => url
      case None      => location
    }
  }

}
