package amf.spec.raml

import amf.compiler.RamlFragmentHeader._
import amf.compiler.{RamlFragment, Root}
import amf.document.Fragment._
import amf.domain.Annotation.SourceVendor
import amf.domain.`abstract`.{ResourceType, Trait}
import amf.domain.extensions.CustomDomainProperty
import amf.domain.{Annotations, ExternalDomainElement}
import amf.parser._
import amf.plugins.domain.webapi.contexts.WebApiContext
import amf.remote.Raml
import amf.shape.Shape
import amf.spec.ParserContext
import amf.spec.declaration._
import amf.spec.domain.RamlNamedExampleParser
import org.yaml.model.{YMap, YType}

/**
  *
  */
case class RamlFragmentParser(root: Root,  fragmentType: RamlFragment)(implicit val ctx: WebApiContext) extends RamlSpecParser {

  def parseFragment(): Fragment = {
    // first i must identify the type of fragment

    val rootMap: YMap = root.document.to[YMap] match {
      case Right(map) => map
      case _ =>
        ctx.violation(root.location, "Cannot parse empty map", root.document)
        YMap()
    }

    val fragment: Fragment = fragmentType match {
      case Raml10DocumentationItem         => DocumentationItemFragmentParser(rootMap).parse()
      case Raml10DataType                  => DataTypeFragmentParser(rootMap).parse()
      case Raml10ResourceType              => ResourceTypeFragmentParser(rootMap).parse()
      case Raml10Trait                     => TraitFragmentParser(rootMap).parse()
      case Raml10AnnotationTypeDeclaration => AnnotationFragmentParser(rootMap).parse()
      case Raml10SecurityScheme            => SecuritySchemeFragmentParser(rootMap).parse()
      case Raml10NamedExample              => NamedExampleFragmentParser(rootMap).parse()
      case _ =>
        ctx.violation(root.location, "Unsupported fragment type", root.document)
        ExternalFragment().withId(root.location).withEncodes(ExternalDomainElement().withRaw(root.raw))
    }

    UsageParser(rootMap, fragment).parse()

    fragment.add(Annotations(root.document) += SourceVendor(Raml))

    val references = ReferencesParser("uses", rootMap, root.references).parse(root.location)

    if (references.references.nonEmpty) fragment.withReferences(references.solvedReferences())
    fragment
  }

  case class DocumentationItemFragmentParser(map: YMap) {
    def parse(): DocumentationItem = {

      val item = DocumentationItem().adopted(root.location)

      item.withEncodes(RamlCreativeWorkParser(map, withExtention = true).parse())

      item
    }
  }

  case class DataTypeFragmentParser(map: YMap) {
    def parse(): DataType = {
      val dataType = DataType().adopted(root.location)

      RamlTypeParser(
        map,
        "type",
        map,
        (shape: Shape) => shape.adopted(root.location),
        isAnnotation = false,
        StringDefaultType
      ).parse()
       .foreach(dataType.withEncodes)

      dataType
    }
  }

  case class ResourceTypeFragmentParser(map: YMap) {
    def parse(): ResourceTypeFragment = {
      val resourceType = ResourceTypeFragment().adopted(root.location)

      val abstractDeclaration =
        new AbstractDeclarationParser(ResourceType(map), resourceType.id, "resourceType", map).parse()

      resourceType.withEncodes(abstractDeclaration)

    }
  }

  case class TraitFragmentParser(map: YMap) {
    def parse(): TraitFragment = {
      val traitFragment = TraitFragment().adopted(root.location)

      val abstractDeclaration =
        new AbstractDeclarationParser(Trait(map), traitFragment.id, "trait", map).parse()

      traitFragment.withEncodes(abstractDeclaration)
    }
  }

  case class AnnotationFragmentParser(map: YMap) {
    def parse(): AnnotationTypeDeclaration = {
      val annotation = AnnotationTypeDeclaration().adopted(root.location)

      val property =
        AnnotationTypesParser(map,
                              "annotation",
                              map,
                              (annotation: CustomDomainProperty) => annotation.adopted(root.location)).parse()

      annotation.withEncodes(property)
    }
  }

  case class SecuritySchemeFragmentParser(map: YMap) {
    def parse(): SecurityScheme = {
      val security = SecurityScheme().adopted(root.location)

      security.withEncodes(
        RamlSecuritySchemeParser(map,
                                 "securityDefinitions",
                                 map,
                                 (security: amf.domain.security.SecurityScheme) => security.adopted(root.location))
          .parse())
    }
  }

  case class NamedExampleFragmentParser(map: YMap) {
    def parse(): NamedExample = {
      val entries      = map.entries.filter(e => e.value.tagType == YType.Map)
      val namedExample = NamedExample().adopted(root.location)

      if (entries.size == 1) namedExample.withEncodes(RamlNamedExampleParser(entries.head).parse())
      else
        throw new IllegalStateException(
          "Could not identified the named example in fragment because it contains more than one named map.")
    }
  }

}
