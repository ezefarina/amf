package amf.convert

import _root_.org.scalatest.{Assertion, AsyncFunSuite, Matchers}
import amf._
import amf.client.AMF
import amf.client.convert.CoreClientConverters._
import amf.client.convert.NativeOps
import amf.client.environment.{DefaultEnvironment, Environment}
import amf.client.model.document._
import amf.client.model.domain._
import amf.client.parse._
import amf.client.remote.Content
import amf.client.render.{Renderer, _}
import amf.client.resolve.{Raml08Resolver, Raml10Resolver}
import amf.client.resource.{ResourceLoader, ResourceNotFound}
import amf.common.Diff
import amf.core.exception.UnsupportedVendorException
import amf.core.parser.Range
import amf.core.remote.{Aml, Oas20, Raml10}
import amf.core.vocabulary.Namespace
import amf.core.vocabulary.Namespace.Xsd
import amf.plugins.document.Vocabularies
import org.mulesoft.common.io.{LimitReachedException, LimitedStringBuffer}
import org.yaml.builder.JsonOutputBuilder

import scala.concurrent.{ExecutionContext, Future}

trait WrapperTests extends AsyncFunSuite with Matchers with NativeOps {

  override implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  private val banking       = "file://amf-client/shared/src/test/resources/production/raml10/banking-api/api.raml"
  private val zencoder      = "file://amf-client/shared/src/test/resources/api/zencoder.raml"
  private val zencoder08    = "file://amf-client/shared/src/test/resources/api/zencoder08.raml"
  private val music         = "file://amf-client/shared/src/test/resources/production/world-music-api/api.raml"
  private val demosDialect  = "file://amf-client/shared/src/test/resources/api/dialects/eng-demos.raml"
  private val demos2Dialect = "file://amf-client/shared/src/test/resources/api/dialects/eng-demos-2.raml"
  private val demosInstance = "file://amf-client/shared/src/test/resources/api/examples/libraries/demo.raml"
  private val security      = "file://amf-client/shared/src/test/resources/upanddown/unnamed-security-scheme.raml"
  private val amflight =
    "file://amf-client/shared/src/test/resources/production/raml10/american-flight-api-2.0.1-raml.ignore/api.raml"
  private val defaultValue = "file://amf-client/shared/src/test/resources/api/shape-default.raml"
  private val profile      = "file://amf-client/shared/src/test/resources/api/validation/custom-profile.raml"
  //  private val banking       = "file://amf-client/shared/src/test/resources/api/banking.raml"
  private val aml_doc = "file://vocabularies/vocabularies/aml_doc.yaml"
  private val scalarAnnotations =
    "file://amf-client/shared/src/test/resources/org/raml/parser/annotation/scalar-nodes/input.raml"

  test("Parsing raml 1.0 test (detect)") {
    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser().parseFileAsync(zencoder).asFuture
    } yield {
      assertBaseUnit(unit, zencoder)
    }
  }

  test("Parsing raml 0.8 test (detect)") {
    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser().parseFileAsync(zencoder08).asFuture
    } yield {
      assertBaseUnit(unit, zencoder08)
    }
  }

  test("Parsing raml 1.0 test") {
    for {
      _    <- AMF.init().asFuture
      unit <- new Raml10Parser().parseFileAsync(zencoder).asFuture
    } yield {
      assertBaseUnit(unit, zencoder)
    }
  }

  test("Parsing raml 0.8 test") {
    for {
      _    <- AMF.init().asFuture
      unit <- new Raml08Parser().parseFileAsync(zencoder08).asFuture
    } yield {
      assertBaseUnit(unit, zencoder08)
    }
  }

  test("Parsing default value string") {
    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser().parseFileAsync(defaultValue).asFuture
    } yield {
      val declares = unit.asInstanceOf[DeclaresModel].declares.asSeq
      assert(declares.size == 1)
      assert(declares.head.isInstanceOf[NodeShape])
      val shape = declares.head.asInstanceOf[NodeShape]
      assert(
        shape.defaultValueStr
          .value()
          .equals("\n      name: roman\n      lastname: riquelme\n      age: 39".stripMargin))
      assert(shape.defaultValue.isInstanceOf[ObjectNode])
    }
  }

  test("Render / parse test RAML 0.8") {
    for {
      _      <- AMF.init().asFuture
      unit   <- new RamlParser().parseFileAsync(zencoder08).asFuture
      output <- new Raml08Renderer().generateString(unit).asFuture
      result <- new Raml08Parser().parseStringAsync(output).asFuture
    } yield {
      assertBaseUnit(result, "http://a.ml/amf/default_document")
    }
  }

  test("Render / parse test RAML 1.0") {
    for {
      _      <- AMF.init().asFuture
      unit   <- new RamlParser().parseFileAsync(zencoder).asFuture
      output <- new Raml10Renderer().generateString(unit).asFuture
      result <- new Raml10Parser().parseStringAsync(output).asFuture
    } yield {
      assertBaseUnit(result, "http://a.ml/amf/default_document")
    }
  }

  test("Source vendor RAML 1.0") {
    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser().parseFileAsync(zencoder).asFuture
    } yield {
      unit.sourceVendor.asOption should be(Some(Raml10))
    }
  }

  test("Render / parse test OAS 2.0") {
    for {
      _      <- AMF.init().asFuture
      unit   <- new RamlParser().parseFileAsync(zencoder).asFuture
      output <- new Oas20Renderer().generateString(unit).asFuture
      result <- new Oas20Parser().parseStringAsync(output).asFuture
    } yield {
      assertBaseUnit(result, "http://a.ml/amf/default_document")
    }
  }

  test("Render / parse test AMF") {
    for {
      _      <- AMF.init().asFuture
      unit   <- new RamlParser().parseFileAsync(zencoder).asFuture
      output <- new AmfGraphRenderer().generateString(unit).asFuture
      result <- new AmfGraphParser().parseStringAsync(output).asFuture
    } yield {
      assertBaseUnit(result, "http://a.ml/amf/default_document")
    }
  }

  test("Validation test") {
    for {
      _           <- AMF.init().asFuture
      unit        <- new RamlParser().parseFileAsync(zencoder).asFuture
      report      <- AMF.validate(unit, RamlProfile, AMFStyle).asFuture
      profileName <- AMF.loadValidationProfile(profile).asFuture
      custom      <- AMF.validate(unit, profileName, AMFStyle).asFuture
    } yield {
      assert(report.conforms)
      assert(!custom.conforms)
    }
  }

  test("Resolution test") {
    for {
      _        <- AMF.init().asFuture
      unit     <- new RamlParser().parseFileAsync(zencoder).asFuture
      resolved <- Future.successful(AMF.resolveRaml10(unit))
      report   <- AMF.validate(resolved, RamlProfile, AMFStyle).asFuture
    } yield {
      assert(report.conforms)
    }
  }

  test("Vocabularies test") {
    for {
      _           <- AMF.init().asFuture
      dialectName <- AMF.registerDialect(demosDialect).asFuture
      unit        <- new Aml10Parser().parseFileAsync(demosInstance).asFuture
      report      <- AMF.validate(unit, ProfileName("Eng Demos 0.1"), AMFStyle).asFuture
    } yield {
      AMF.registerNamespace("eng-demos", "http://mulesoft.com/vocabularies/eng-demos#")
      val elem = unit.asInstanceOf[DialectInstance].encodes
      assert(elem.definedBy().nodetypeMapping.is("http://mulesoft.com/vocabularies/eng-demos#Presentation"))
      assert(elem.getTypeUris().asSeq.contains("http://mulesoft.com/vocabularies/eng-demos#Presentation"))
      // TODO: fix this getter
//       val res = elem.getDialectObjectsByPropertyId("eng-demos:speakers").asInternal
//      assert(elem.graph().getObjectByPropertyId("eng-demos:speakers").size > 0) // todo ???
      assert(elem.getObjectPropertyUri("eng-demos:speakers").asSeq.size == 2)
    }
  }

  test("Custom Vocabularies test") {
    case class CustomLoader() extends ResourceLoader {
      private val url = "vocab:eng-demos-2.raml"
      private val stream =
        """
          |#%RAML 1.0 Vocabulary
          |
          |# Name of the vocabulary
          |vocabulary: Eng Demos
          |
          |usage: Engineering Demonstrations @ MuleSoft
          |
          |# Namespace for the vocabulary (must be a URI prefix)
          |# All terms in the vocabulary will be URIs in this namespace
          |base: http://mulesoft.com/vocabularies/eng-demos#
          |
          |external:
          |  schema-org: http://schema.org/
          |
          |classTerms:
          |
          |  # URI for this term: http://mulesoft.com/vocabularies/eng-demos#Presentation
          |  Presentation:
          |    displayName: Presentation
          |    description: Product demonstrations
          |    properties:
          |      - showcases
          |      - speakers
          |      - demoDate
          |
          |  Speaker:
          |    displayName: Speaker
          |    description: Product demonstration presenter
          |    extends: schema-org.Person
          |    properties:
          |      - nickName
          |
          |  schema-org.Product:
          |    displayName: Product
          |    description: The product being showcased
          |    properties:
          |      - resources
          |
          |
          |propertyTerms:
          |
          |  # scalar range, datatype property
          |  # URI for this term: http://mulesoft.com/vocabularies/eng-demos#nickName
          |  nickName:
          |    displayName: nick
          |    description: nick name of the speaker
          |    range: string
          |    extends: schema-org.alternateName
          |
          |  showcases:
          |    displayName: showcases
          |    description: Product being showcased in a presentation
          |    range: schema-org.Product
          |
          |  speakers:
          |    displayName: speakers
          |    description: list of speakers
          |    range: Speaker
          |
          |  resources:
          |    displayName: resources
          |    description: list of materials about the showcased product
          |    range: string
          |
          |  semantic-version:
          |    displayName: semantic version
          |    description: 'semantic version standard: M.m.r'
          |    extends: schema-org.version
          |    range: string
          |
          |  demoDate:
          |    displayName: demo date
          |    description: day the demo took place
          |    extends: schema-org.dateCreated
          |    range: date
          |
          |  isRecorded:
          |    displayName: is recorded
          |    description: notifies if this demo was recorded
          |    range: boolean
          |
          |  code:
          |    displayName: code
          |    description: product code
          |    range: string
          |    extends: schema-org.name
        """.stripMargin

      /** Fetch specified resource and return associated content. Resource should have benn previously accepted. */
      override def fetch(resource: String): ClientFuture[Content] = Future { new Content(stream, url) }.asClient

      /** Accepts specified resource. */
      override def accepts(resource: String): Boolean = resource == url
    }

    val env = DefaultEnvironment().add(CustomLoader().asInstanceOf[ClientLoader])

    for {
      _           <- AMF.init().asFuture
      dialectName <- Vocabularies.registerDialect(demos2Dialect, env).asFuture
      unit        <- new Aml10Parser().parseFileAsync(demosInstance).asFuture
      report      <- AMF.validate(unit, ProfileName("Eng Demos 0.1"), AMFStyle).asFuture
    } yield {
      AMF.registerNamespace("eng-demos", "http://mulesoft.com/vocabularies/eng-demos#")
      val elem = unit.asInstanceOf[DialectInstance].encodes
      assert(elem.definedBy().nodetypeMapping.is("http://mulesoft.com/vocabularies/eng-demos#Presentation"))
      assert(elem.getTypeUris().asSeq.contains("http://mulesoft.com/vocabularies/eng-demos#Presentation"))
      assert(elem.getObjectPropertyUri("eng-demos:speakers").asSeq.size == 2)
    }
  }

  test("Raml to oas security scheme after resolution") {
    for {
      _      <- AMF.init().asFuture
      unit   <- new RamlParser().parseFileAsync(security).asFuture
      _      <- Future.successful(new Raml10Resolver().resolve(unit))
      output <- new Oas20Renderer().generateString(unit).asFuture
    } yield {
      assert(!output.isEmpty)
    }
  }

  test("world-music-test") {
    for {
      _      <- AMF.init().asFuture
      unit   <- amf.Core.parser(Raml10.name, "application/yaml").parseFileAsync(music).asFuture
      report <- AMF.validate(unit, RamlProfile, RAMLStyle).asFuture
    } yield {
      assert(!unit.references().asSeq.map(_.location).contains(null))
      assert(report.conforms)
    }
  }

  test("Scalar Annotations") {
    for {
      _    <- AMF.init().asFuture
      unit <- amf.Core.parser(Raml10.name, "application/yaml").parseFileAsync(scalarAnnotations).asFuture
    } yield {
      val api         = unit.asInstanceOf[Document].encodes.asInstanceOf[WebApi]
      val annotations = api.name.annotations().custom().asSeq
      annotations should have size 1
      val annotation = annotations.head
      annotation.name.value() should be("foo")
      annotation.extension.asInstanceOf[ScalarNode].value should be("annotated title")
    }
  }

  test("Vocabulary generation") {

//    import amf.client.convert.VocabulariesClientConverter._ //todo uncomment to import *.asClient

    val vocab = new Vocabulary()
    vocab
      .withName("Vocab")
      .withBase("http://test.com/vocab#")
      .withLocation("test_vocab.raml")
      .withUsage("Just a small sample vocabulary")
    /*.withExternals(
        Seq(
          new External()
            .withAlias("other")
            .withBase("http://test.com/vocabulary/other#")
        ).toClient)
      .withUsages( // todo withImports?
        Seq(
          new VocabularyReference()
            .withAlias("raml-doc")
            .withReference("http://a.ml/vocabularies/doc#")
        ).toClient)*/

    assert(vocab.base.option.asOption.isDefined)
    assert(vocab.base.is("http://test.com/vocab#"))
    assert(vocab.description.option.asOption.isDefined)
    assert(vocab.description.is("Just a small sample vocabulary"))

    val propertyTerm = new DatatypePropertyTerm()
      .withId("http://a.ml/vocabularies/doc#test")
      .withRange("http://www.w3.org/2001/XMLSchema#string")

    val classTerm = new ClassTerm()
      .withId("http://test.com/vocab#Class")
      .withDescription("A sample class")
      .withDisplayName("Class")
//      .withSubClassOf(Seq("http://test.com/vocabulary/other#Class").asClient)
//      .withProperties(Seq("http://a.ml/vocabularies/doc#test").asClient)

    vocab.withDeclaredElement(classTerm).withDeclaredElement(propertyTerm)

    for {
      _      <- AMF.init().asFuture
      render <- amf.Core.generator("RAML Vocabulary", "application/yaml").generateString(vocab).asFuture
    } yield {
      render should be(
        """#%Vocabulary 1.0
          |base: http://test.com/vocab#
          |vocabulary: Vocab
          |usage: Just a small sample vocabulary
          |classTerms:
          |  Class:
          |    displayName: Class
          |    description: A sample class
          |propertyTerms:
          |  test:
          |    range: string
          |""".stripMargin
      )
    }
    /*
      text ==
        """#%RAML 1.0 Vocabulary
        |base: http://test.com/vocab#
        |version: 1.0
        |usage: Just a small sample vocabulary
        |external:
        |  other: http://test.com/vocabulary/other#
        |uses:
        |  raml-doc: http://a.ml/vocabularies/doc#
        |classTerms:
        |  Class:
        |    displayName: Class
        |    description: A sample class
        |    extends: other.Class
        |    properties: raml-doc.test
        |propertyTerms:
        |  raml-doc.test:
        |    range: string
        |""".stripMargin)
   */
  }

  /*
  TODO: Fix setters
  test("vocabularies parsing ranges") {
    amf.plugins.document.Vocabularies.register()
    amf.plugins.document.WebApi.register()
    amf.Core.init().get()

    val parser                               = amf.Core.parser("RAML Vocabularies", "application/yaml")
    val parsed                               = parser.parseFileAsync("file://vocabularies/vocabularies/raml_shapes.raml").get()
    val vocabulary                           = parsed.asInstanceOf[Vocabulary]
    val acc: mutable.HashMap[String, String] = new mutable.HashMap()
    for {
      objectProperties   <- vocabulary.objectPropertyTerms()
      dataTypeProperties <- vocabulary.datatypePropertyTerms()
    } yield {
      acc.put(property.getId(), range)
    }

    assert(acc.size == 14)
  }
   */

  test("Vocabularies parsing aml_doc") {
    for {
      _    <- AMF.init().asFuture
      unit <- amf.Core.parser(Aml.name, "application/yaml").parseFileAsync(aml_doc).asFuture
    } yield {
      val declarations = unit.asInstanceOf[Vocabulary].declares.asSeq

      val classes    = declarations.collect { case term: ClassTerm    => term }
      val properties = declarations.collect { case prop: PropertyTerm => prop }

      assert(classes.size == 19)
      assert(properties.size == 28)
    }
  }

  test("Parsing text document with base url") {
    val baseUrl = "http://test.com/myApp"
    testParseStringWithBaseUrl(baseUrl)
  }

  test("Parsing text document with base url (domain only)") {
    val baseUrl = "http://test.com"
    testParseStringWithBaseUrl(baseUrl)
  }

  test("Environment test") {
    val include = "amf://types/Person.raml"

    val input = s"""
      |#%RAML 1.0
      |title: Environment test
      |types:
      |  Person: !include $include
    """.stripMargin

    val person = """
      |#%RAML 1.0 DataType
      |type: object
      |properties:
      |  name: string
    """.stripMargin

    case class TestResourceLoader() extends ResourceLoader {

      import amf.client.convert.WebApiClientConverters._

      override def fetch(resource: String): ClientFuture[Content] =
        Future.successful(new Content(person, resource)).asClient

      override def accepts(resource: String): Boolean = resource == include
    }

    val environment = Environment.empty().add(TestResourceLoader().asInstanceOf[ClientLoader])

    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser(environment).parseStringAsync(input).asFuture
    } yield {
      unit shouldBe a[Document]
      val declarations = unit.asInstanceOf[Document].declares.asSeq
      declarations should have size 1
    }
  }

  test("Environment returning bad uri test") {
    val name = "api.raml"

    val input = s"""
                   |#%RAML 1.0
                   |title: Environment test
    """.stripMargin

    val name2 = "api2"

    case class BadIRIResourceLoader() extends ResourceLoader {

      import amf.client.convert.WebApiClientConverters._

      override def fetch(resource: String): ClientFuture[Content] =
        Future.successful(new Content(input, resource)).asClient

      override def accepts(resource: String): Boolean = true
    }

    val environment = Environment.empty().add(BadIRIResourceLoader().asInstanceOf[ClientLoader])

    for {
      _     <- AMF.init().asFuture
      unit  <- new RamlParser(environment).parseFileAsync(name).asFuture
      unit2 <- new RamlParser(environment).parseFileAsync(name2).asFuture
    } yield {
      unit shouldBe a[Document]
      unit.id should be("file://api.raml")
      unit2.id should be("file://api2")
    }
  }

  test("Generate to writer and exit") {
    val input = s"""
                   |#%RAML 1.0
                   |title: Environment test
                   |version: 32.0.7
    """.stripMargin

    val buffer = LimitedStringBuffer(450)
    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser().parseStringAsync(input).asFuture
      e    <- new AmfGraphRenderer().generateToWriter(unit, buffer).asFuture.failed
    } yield {
      e shouldBe a[LimitReachedException]

      buffer.toString() should endWith("http://a.ml/vocabularies/document#RootDomainElement\",\n")
    }
  }

  test("Generate to doc builder") {
    val input = s"""
                   |#%RAML 1.0
                   |title: Environment test
                   |version: 32.0.7
    """.stripMargin

    val builder = JsonOutputBuilder()
    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser().parseStringAsync(input).asFuture
      e    <- new AmfGraphRenderer().generateToBuilder(unit, builder).asFuture
    } yield {
      builder.result.toString should include("\"http://schema.org/version\"")
    }
  }

  test("Environment resource not loaded exception") {
    val name = "api.raml"

    val input = s"""
                   |#%RAML 1.0
                   |title: Environment test
                   |types:
                   |  A: !include not-exists.raml
    """.stripMargin

    case class ForFailResourceLoader() extends ResourceLoader {

      import amf.client.convert.WebApiClientConverters._

      override def fetch(resource: String): ClientFuture[Content] = {
        val f =
          if (resource.endsWith("api.raml")) Future.successful(new Content(input, resource))
          else
            Future.failed(new ResourceNotFound(s"Cannot find resource $resource"))

        f.asClient
      }

      override def accepts(resource: String): Boolean = true
    }

    val environment = Environment.empty().add(ForFailResourceLoader().asInstanceOf[ClientLoader])

    for {
      _      <- AMF.init().asFuture
      unit   <- new RamlParser(environment).parseFileAsync(name).asFuture
      report <- AMF.validate(unit, Raml10Profile, RAMLStyle).asFuture
    } yield {
      report.conforms should be(false)
      report.results.asSeq.exists(_.message.equals("Cannot find resource not-exists.raml")) should be(true)
    }
  }

  test("Environment fallback test") {
    val include = "amf://types/Person.raml"

    val input = s"""
       |#%RAML 1.0
       |title: Environment test
       |types:
       |  Person: !include $include
    """.stripMargin

    val person = """
       |#%RAML 1.0 DataType
       |type: object
       |properties:
       |  name: string
     """.stripMargin

    import amf.client.convert.WebApiClientConverters._

    case class TestResourceLoader() extends ResourceLoader {
      override def fetch(resource: String): ClientFuture[Content] =
        Future.successful(new Content(person, resource)).asClient

      override def accepts(resource: String): Boolean = resource == include
    }

    case class FailingResourceLoader(msg: String) extends ResourceLoader {
      override def fetch(resource: String): ClientFuture[Content] =
        Future.failed[Content](new Exception(msg)).asClient
    }

    val environment = Environment
      .empty()
      .add(TestResourceLoader().asInstanceOf[ClientLoader])
      .add(FailingResourceLoader("Unreachable network").asInstanceOf[ClientLoader])
      .add(FailingResourceLoader("Invalid protocol").asInstanceOf[ClientLoader])

    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser(environment).parseStringAsync(input).asFuture
    } yield {
      unit shouldBe a[Document]
      val declarations = unit.asInstanceOf[Document].declares.asSeq
      declarations should have size 1
    }
  }

  test("Missing converter error") {
    val options = new RenderOptions().withoutSourceMaps

    for {
      _        <- AMF.init().asFuture
      unit     <- amf.Core.parser(Raml10.name, "application/yaml").parseFileAsync(amflight).asFuture
      resolved <- Future.successful(AMF.resolveRaml10(unit))
    } yield {
      val webapi = resolved.asInstanceOf[Document].encodes.asInstanceOf[WebApi]
      webapi.endPoints.asSeq.foreach { ep =>
        ep.operations.asSeq.foreach { op =>
          op.responses.asSeq.foreach { resp =>
            resp.payloads.asSeq.foreach { payload =>
              payload.schema
            }
          }
        }
      }
      assert(true)
    }
  }

  test("Build shape without default value") {

    val shape = new ScalarShape()
    shape.withDataType("string")
    shape.withName("name")

    assert(shape.defaultValue == null)
  }

  test("Remove field and dump") {
    val api =
      """
        |#%RAML 1.0
        |title: this should remain
        |description: remove
        |license:
        | url: removeUrl
        | name: test
        |/endpoint1:
        | get:
        |   responses:
        |     200:
      """.stripMargin

    val excepted =
      """
        |#%RAML 1.0
        |title: this should remain
        |/endpoint1:
        | get: {}""".stripMargin
    for {
      _         <- AMF.init().asFuture
      unit      <- new RamlParser().parseStringAsync(api).asFuture
      removed   <- removeFields(unit)
      generated <- AMF.raml10Generator().generateString(removed).asFuture
    } yield {
      val deltas = Diff.ignoreAllSpace.diff(excepted, generated)
      if (deltas.nonEmpty) fail("Expected and golden are different: " + Diff.makeString(deltas))
      else succeed
    }
  }

  test("Test swagger 2.0 entry generation in yaml") {
    val expected =
      """
        |swagger: "2.0"
        |info:
        | title: test swagger entry
        | version: "1.0"
        |paths:
        | /endpoint:
        |  get:
        |    responses:
        |      "200":
        |       description: a descrip""".stripMargin
    for {
      _         <- AMF.init().asFuture
      doc       <- Future { buildBasicApi() }
      generated <- new Renderer(Oas20.name, "application/yaml").generateString(doc).asFuture
    } yield {
      val deltas = Diff.ignoreAllSpace.diff(expected, generated)
      if (deltas.nonEmpty) fail("Expected and golden are different: " + Diff.makeString(deltas))
      else succeed
    }
  }

  test("Test swagger ref generation in yaml") {
    val expected =
      """|swagger: "2.0"
         |info:
         |  title: test swagger entry
         |  version: "1.0"
         |paths:
         |  /endpoint:
         |    get:
         |      parameters:
         |        -
         |          x-amf-mediaType: application/json
         |          in: body
         |          name: someName
         |          schema:
         |            $ref: "#/definitions/person"
         |      responses:
         |        "200":
         |          description: a descrip
         |definitions:
         |  person:
         |    type: object
         |    properties:
         |      name:
         |        type: string""".stripMargin
    for {
      _         <- AMF.init().asFuture
      doc       <- Future { buildApiWithTypeTarget() }
      generated <- new Renderer(Oas20.name, "application/yaml").generateString(doc).asFuture
    } yield {
      val deltas = Diff.ignoreAllSpace.diff(expected, generated)
      if (deltas.nonEmpty) fail("Expected and golden are different: " + Diff.makeString(deltas))
      else succeed
    }
  }

  test("Test any shape default empty") {
    val api =
      """
        |#%RAML 1.0
        |title: test swagger entry
        |/endpoint:
        |   get:
        |     body:
        |       application/json:
        |   post:
        |     body:
        |       application/json:
        |           example: |
        |             { "name": "roman", "lastname": "riquelme"}
        |   put:
        |     body:
        |       application/json:
        |           type: any
        |           example: |
        |             { "name": "roman", "lastname": "riquelme"}
        |   patch:
        |     body:
        |       application/json:
        |           type: object
        |           example: |
        |             { "name": "roman", "lastname": "riquelme"}
        |   delete:
        |     body:
        |       application/json:
        |           type: string""".stripMargin
    for {
      _   <- AMF.init().asFuture
      doc <- AMF.ramlParser().parseStringAsync(api).asFuture
    } yield {

      val seq = doc.asInstanceOf[Document].encodes.asInstanceOf[WebApi].endPoints.asSeq.head.operations.asSeq
      def assertDefault(method: String, expected: Boolean) =
        seq
          .find(_.method.value().equals(method))
          .get
          .request
          .payloads
          .asSeq
          .head
          .schema
          .asInstanceOf[AnyShape]
          .isDefaultEmpty should be(expected)

      assertDefault("get", expected = true)
      assertDefault("post", expected = true)
      assertDefault("put", expected = false)
      assertDefault("patch", expected = false)
      assertDefault("delete", expected = false)

    }
  }

  private def buildBasicApi() = {
    val api: WebApi = new WebApi().withName("test swagger entry")

    api.withEndPoint("/endpoint").withOperation("get").withResponse("200").withDescription("a descrip")
    new Document().withEncodes(api)

  }

  private def buildApiWithTypeTarget() = {
    val doc = buildBasicApi()

    val shape     = new ScalarShape().withDataType((Xsd + "string").iri())
    val nodeShape = new NodeShape().withName("person")
    nodeShape.withProperty("name").withRange(shape)
    doc.withDeclaredElement(nodeShape)

    val linked: NodeShape = nodeShape.link(Some("#/definitions/person"))
    linked.withName("Person")
    doc.encodes
      .asInstanceOf[WebApi]
      .endPoints
      .asSeq
      .head
      .operations
      .asSeq
      .head
      .withRequest()
      .withPayload("application/json")
      .withName("someName")
      .withSchema(linked)
    doc
  }

  test("Test dynamic types") {
    val api =
      """
        |#%RAML 1.0
        |title: this should remain
        |description: remove
        |license:
        | url: removeUrl
        | name: test
        |/endpoint1:
        | get:
        |   responses:
        |     200:
        |       body:
        |        application/json:
        |           properties:
        |             name: string
        |             lastname: string
        |           example:
        |             name: roman
        |             lastname: riquelme
        |
      """.stripMargin

    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser().parseStringAsync(api).asFuture
    } yield {
      val webApi = unit.asInstanceOf[Document].encodes.asInstanceOf[WebApi]
      val dataNode = webApi.endPoints.asSeq.head.operations.asSeq.head.responses.asSeq.head.payloads.asSeq.head.schema
        .asInstanceOf[AnyShape]
        .examples
        .asSeq
        .head
        .structuredValue
      assert(dataNode._internal.meta.`type`.head.iri() == (Namespace.Data + "Object").iri())
    }
  }

  test("Test name in property shape") {
    val api =
      """
        |#%RAML 1.0
        |title: this should remain
        |
        |types:
        | person:
        |   properties:
        |     name: string
      """.stripMargin

    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser().parseStringAsync(api).asFuture
    } yield {
      val nodeShape = unit.asInstanceOf[Document].declares.asSeq.head.asInstanceOf[NodeShape]
      nodeShape.properties.asSeq.head.name.value() should be("name")
    }
  }

  test("Test order of uri parameter") {
    val api =
      """
        |#%RAML 1.0
        |---
        |title: RAML 1.0 Uri param
        |version: v1
        |
        |/part:
        |  get:
        |  /{uriParam1}:
        |    uriParameters:
        |      uriParam1:
        |        type: integer
        |    get:
        |    /{uriParam2}:
        |      uriParameters:
        |        uriParam2:
        |          type: number
        |      get:
        |      /{uriParam3}:
        |        uriParameters:
        |          uriParam3:
        |            type: boolean
        |        get:
      """.stripMargin

    for {
      _        <- AMF.init().asFuture
      unit     <- new RamlParser().parseStringAsync(api).asFuture
      resolved <- Future { new Raml10Resolver().resolve(unit) }
    } yield {
      val pathParamters: List[Parameter] = resolved
        .asInstanceOf[Document]
        .encodes
        .asInstanceOf[WebApi]
        .endPoints
        .asSeq
        .last
        .parameters
        .asSeq
        .filter(_.binding.value().equals("path"))
        .toList

      assert(pathParamters.head.name.value().equals("uriParam1"))
      assert(pathParamters(1).name.value().equals("uriParam2"))
      assert(pathParamters(2).name.value().equals("uriParam3"))

    }
  }

  test("Test order of base uri parameter ") {
    val api =
      """
        |#%RAML 1.0
        |---
        |title: RAML 1.0 Uri param
        |version: v1
        |baseUri: https://www.example.com/api/{v1}/{v2}
        |
        |baseUriParameters:
        |  v2:
        |  v1:
        |    type: string""".stripMargin

    for {
      _        <- AMF.init().asFuture
      unit     <- new RamlParser().parseStringAsync(api).asFuture
      resolved <- Future { new Raml10Resolver().resolve(unit) }
    } yield {
      val baseParameters: Seq[Parameter] =
        resolved.asInstanceOf[Document].encodes.asInstanceOf[WebApi].servers.asSeq.head.variables.asSeq

      assert(baseParameters.head.name.value().equals("v1"))
      assert(baseParameters(1).name.value().equals("v2"))

    }
  }

  test("Test order of raml 08 form parameters ") {
    val api =
      """
        |#%RAML 0.8
        |---
        |title: RAML 1.0 Uri param
        |version: v1
        |
        |/multipart:
        |  post:
        |    body:
        |      multipart/form-data:
        |        formParameters:
        |          first:
        |            type: string
        |            required: true
        |          second:
        |            type: string
        |            default: segundo
        |          third:
        |            type: boolean
        |    responses:
        |      201: ~
      """.stripMargin

    for {
      _        <- AMF.init().asFuture
      unit     <- new RamlParser().parseStringAsync(api).asFuture
      resolved <- Future { new Raml08Resolver().resolve(unit) }
    } yield {
      val shape: Shape = unit
        .asInstanceOf[Document]
        .encodes
        .asInstanceOf[WebApi]
        .endPoints
        .asSeq
        .head
        .operations
        .asSeq
        .head
        .request
        .payloads
        .asSeq
        .head
        .schema

      assert(shape.isInstanceOf[NodeShape])
      val properties = shape.asInstanceOf[NodeShape].properties.asSeq
      assert(properties.head.name.value().equals("first"))
      assert(properties(1).name.value().equals("second"))
      assert(properties(2).name.value().equals("third"))
    }
  }

  private def removeFields(unit: BaseUnit): Future[BaseUnit] = Future {
    val webApi = unit.asInstanceOf[Document].encodes.asInstanceOf[WebApi]
    webApi.description.remove()
    val operation: Operation = webApi.endPoints.asSeq.head.operations.asSeq.head
    operation.graph().remove("http://www.w3.org/ns/hydra/core#returns")

    webApi.graph().remove("http://schema.org/license")
    unit
  }

  private def testParseStringWithBaseUrl(baseUrl: String) = {
    val spec =
      """#%RAML 1.0
        |
        |title: Some title
        |version: 0.1
        |
        |/test:
        |  get:
        |    responses:
        |      200:
        |        body:
        |          application/json:
        |            properties:
        |              a: string""".stripMargin

    for {
      _    <- AMF.init().asFuture
      unit <- amf.Core.parser(Raml10.name, "application/yaml").parseStringAsync(baseUrl, spec).asFuture
    } yield {
      assert(unit.location.startsWith(baseUrl))
      val encodes = unit.asInstanceOf[Document].encodes
      assert(encodes.id.startsWith(baseUrl))
      assert(encodes.asInstanceOf[WebApi].name.is("Some title"))
    }

  }

  private def assertBaseUnit(baseUnit: BaseUnit, expectedLocation: String): Assertion = {
    assert(baseUnit.location == expectedLocation)
    val api       = baseUnit.asInstanceOf[Document].encodes.asInstanceOf[WebApi]
    val endPoints = api.endPoints.asSeq
    assert(endPoints.size == 1)

    val endpoint = endPoints.head
    assert(endpoint.path.is("/v3.5/path"))
    val ops = endpoint.operations.asSeq
    assert(ops.size == 1)
    val post = ops.head
    assert(post.method.is("get"))
    val payloads = post.request.payloads.asSeq
    assert(payloads.size == 1)

    val first = payloads.head
    assert(first.mediaType.is("application/json"))

    val typeIds = first.schema.graph().types().asSeq
    assert(typeIds.contains("http://a.ml/vocabularies/shapes#ScalarShape"))
    assert(typeIds.contains("http://www.w3.org/ns/shacl#Shape"))
    assert(typeIds.contains("http://a.ml/vocabularies/shapes#Shape"))
    assert(typeIds.contains("http://a.ml/vocabularies/document#DomainElement"))

    val responses = post.responses.asSeq
    assert(
      responses.head.payloads.asSeq.head.schema
        .asInstanceOf[ScalarShape]
        .dataType
        .is("http://www.w3.org/2001/XMLSchema#string"))

    assert(
      payloads.head.schema
        .asInstanceOf[ScalarShape]
        .dataType
        .is("http://www.w3.org/2001/XMLSchema#string"))

    assert(responses.head.statusCode.is("200"))
  }

  test("Test validate payload with invalid iri") {
    val payload = """test payload""".stripMargin
    for {
      _ <- AMF.init().asFuture
      shape <- Future {
        new ScalarShape()
          .withDataType("http://www.w3.org/2001/XMLSchema#string")
          .withName("test")
          .withId("api.raml/#/webapi/schema1")
      }
      report <- shape.asInstanceOf[AnyShape].validate(payload).asFuture
    } yield {
      assert(report.conforms)
    }
  }

  test("Generate unit with source maps") {
    val options = new RenderOptions().withSourceMaps

    for {
      _      <- AMF.init().asFuture
      unit   <- amf.Core.parser(Raml10.name, "application/yaml").parseFileAsync(banking).asFuture
      jsonld <- amf.Core.generator("AMF Graph", "application/ld+json").generateString(unit, options).asFuture
    } yield {
      jsonld should include("[(3,0)-(252,0)]")
    }
  }

  test("Generate unit without source maps") {
    val options = new RenderOptions().withoutSourceMaps

    for {
      _      <- AMF.init().asFuture
      unit   <- amf.Core.parser(Raml10.name, "application/yaml").parseFileAsync(banking).asFuture
      jsonld <- amf.Core.generator("AMF Graph", "application/ld+json").generateString(unit, options).asFuture
    } yield {
      jsonld should not include "[(3,0)-(252,0)]"
    }
  }

  test("Generate unit with compact uris") {
    val options = new RenderOptions().withCompactUris.withSourceMaps

    for {
      _      <- AMF.init().asFuture
      unit   <- amf.Core.parser(Raml10.name, "application/yaml").parseFileAsync(banking).asFuture
      jsonld <- amf.Core.generator("AMF Graph", "application/ld+json").generateString(unit, options).asFuture
    } yield {
      jsonld should include("@context")
    }
  }

  test("banking-api-test") {
    for {
      _    <- AMF.init().asFuture
      unit <- amf.Core.parser(Raml10.name, "application/yaml").parseFileAsync(banking).asFuture
    } yield {
      val references = unit.references().asSeq
      assert(!references.map(_.location).contains(null))
      val traits = references.find(ref => ref.location.endsWith("traits.raml")).head.references().asSeq
      val first  = traits.head
      assert(first.location != null)
      assert(first.asInstanceOf[TraitFragment].encodes != null)
      assert(!traits.map(_.location).contains(null))
    }
  }

  test("Parsing external xml shape") {
    for {
      _ <- AMF.init().asFuture
      unit <- new RamlParser()
        .parseFileAsync("file://amf-client/shared/src/test/resources/production/raml10/xsdschema/api.raml")
        .asFuture
    } yield {
      val location: Option[String] =
        unit.asInstanceOf[Document].declares.asSeq.head.asInstanceOf[SchemaShape].location.asOption
      location.isDefined should be(true)
      location.get should be("file://amf-client/shared/src/test/resources/production/raml10/xsdschema/schema.xsd")

    }
  }

  test("Parsing external xml example") {
    for {
      _ <- AMF.init().asFuture
      unit <- new RamlParser()
        .parseFileAsync("file://amf-client/shared/src/test/resources/production/raml10/xsdexample/api.raml")
        .asFuture
    } yield {
      val location: Option[String] = unit
        .asInstanceOf[Document]
        .declares
        .asSeq
        .head
        .asInstanceOf[AnyShape]
        .examples
        .asSeq
        .head
        .location
        .asOption
      location.isDefined should be(true)
      location.get should be("file://amf-client/shared/src/test/resources/production/raml10/xsdexample/example.xsd")
    }
  }

  test("Parsing external xml with inner ref annotation") {
    for {
      _ <- AMF.init().asFuture
      unit <- new RamlParser()
        .parseFileAsync(
          "file://amf-client/shared/src/test/resources/production/raml10/xsdschema-withfragmentref/api.raml")
        .asFuture
    } yield {
      val shape = unit
        .asInstanceOf[Document]
        .declares
        .asSeq
        .head
        .asInstanceOf[AnyShape]
      shape.isInstanceOf[SchemaShape] should be(true)
      shape.annotations().fragmentName().asOption.get should be("address")
    }
  }

  test("Parsing external json with inner ref annotation") {
    for {
      _ <- AMF.init().asFuture
      unit <- new RamlParser()
        .parseFileAsync(
          "file://amf-client/shared/src/test/resources/production/raml10/jsonschema-apiwithfragmentref/api.raml")
        .asFuture
    } yield {
      val shape = unit
        .asInstanceOf[Document]
        .declares
        .asSeq
        .head
        .asInstanceOf[AnyShape]
      shape.annotations().fragmentName().asOption.get should be("/definitions/address")
    }
  }

  test("Test validate with typed enum amf pair method") {
    for {
      _ <- AMF.init().asFuture
      unit <- new Raml10Parser()
        .parseFileAsync(scalarAnnotations)
        .asFuture
      v <- AMF.validate(unit, RamlProfile, RamlProfile.messageStyle).asFuture
    } yield {
      assert(v.conforms)
    }
  }

  // in fact the change were do it at parsing time (abstract declaration parser). I change the hashmap for a list map of the properties to preserve order, so this test could be parse and dump but i wanna be sure that nobody will change the resolved params order in any other place.
  test("Test query parameters order") {
    for {
      _ <- AMF.init().asFuture
      unit <- new Raml08Parser()
        .parseFileAsync("file://amf-client/shared/src/test/resources/clients/params-order.raml")
        .asFuture
      v <- Future.successful(new Raml08Resolver().resolve(unit))
    } yield {
      val seq = v
        .asInstanceOf[Document]
        .encodes
        .asInstanceOf[WebApi]
        .endPoints
        .asSeq
        .head
        .operations
        .asSeq
        .head
        .request
        .queryParameters
        .asSeq
      seq.head.name.value() should be("code")
      seq(1).name.value() should be("size")
      seq(2).name.value() should be("color")
      seq(3).name.value() should be("description")

    }
  }

  // extract to some kind of client tests in another proyect?
  test("Test custom domain property id after parse") {

    for {
      _ <- AMF.init().asFuture
      doc <- Future {
        val ns            = (Namespace.Xsd + "string").iri()
        val doc: Document = new Document()
        doc._internal.withId("http://location.com/myfile")
        val shape = new ScalarShape().withName("scalarDeclared").withDataType(ns)
        doc.withDeclaredElement(shape)
        val wa = new WebApi().withName("test")
        doc.withEncodes(wa)
        val annotationType =
          new CustomDomainProperty()
            .withName("forDescribedBy")
            .withId("http://location.com/myfile#/declarations/annotations/forDescribedBy")
            .withSchema(new ScalarShape().withName("scalarName").withDataType(ns))
        doc.withDeclaredElement(annotationType)
        val annotation = amf.core.model.domain.extensions
          .DomainExtension()
          .withExtension(new ScalarNode("extension", ns)._internal)
          .withDefinedBy(annotationType._internal)
          .withName(annotationType.name.value())
        shape.withCustomDomainProperties(Seq(annotation).asClient)
        doc
      }
      s      <- new Raml10Renderer().generateString(doc).asFuture
      parsed <- new Raml10Parser().parseStringAsync("http://location.com/myfile", s).asFuture
    } yield {
      val buildedProp: CustomDomainProperty =
        doc.declares.asSeq.collectFirst({ case s: Shape => s.customDomainProperties.asSeq.head.definedBy }).get

      val parsedProp: CustomDomainProperty = parsed
        .asInstanceOf[Document]
        .declares
        .asSeq
        .collectFirst({ case s: Shape => s.customDomainProperties.asSeq.head.definedBy })
        .get
      parsedProp.id should be(buildedProp.id)
    }
  }

  test("Handle 404 status code while fetching included file") {
    for {
      _ <- AMF.init().asFuture
      a <- AMF
        .raml08Parser()
        .parseFileAsync(
          "file://amf-client/shared/src/test/resources/parser-results/raml/error/not-existing-http-include.raml")
        .asFuture
      r <- AMF.validate(a, Raml08Profile, RAMLStyle).asFuture
    } yield {
      r.conforms should be(false)
      val seq = r.results.asSeq
      seq.size should be(2)
      val statusCode = seq.head
      statusCode.level should be("Violation")

      // hack to avoid that this test fail when you don't have internet connection.If you have internet, the a.ml domain will return an 404 error,
      // but if you dont have internet connection, you will not reach the a.ml host, so it will be an unknown host exception violation.

      statusCode.message should (endWith("Unexpected status code '404' for resource 'https://a.ml/notexists'") or
        endWith("Network Error: a.ml") or
        endWith("java.net.SocketTimeoutException: connect timed out") or
        endWith(
          "javax.net.ssl.SSLHandshakeException: sun.security.validator.ValidatorException: PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested target"))
      statusCode.position should be(Range((6, 10), (6, 41)))

      val unresolvedRef = seq.last
      unresolvedRef.level should be("Violation")
      unresolvedRef.message should startWith("Unresolved reference 'https://a.ml/notexists' from root context")
      unresolvedRef.position should be(Range((6, 10), (6, 41)))
    }
  }

  test("Test search tracked example") {
    for {
      _ <- AMF.init().asFuture
      a <- AMF
        .raml10Parser()
        .parseFileAsync("file://amf-client/shared/src/test/resources/resolution/payloads-examples-resolution.raml")
        .asFuture
    } yield {
      val r          = new Raml10Resolver().resolve(a)
      val operations = r.asInstanceOf[Document].encodes.asInstanceOf[WebApi].endPoints.asSeq.head.operations.asSeq
      val getOp      = operations.find(_.method.value().equals("get")).get
      val option = getOp.request.payloads.asSeq.head.schema
        .asInstanceOf[AnyShape]
        .trackedExample(
          "file://amf-client/shared/src/test/resources/resolution/payloads-examples-resolution.raml#/web-api/end-points/%2Fendpoint1/get/request/application%2Fjson")
        .asOption
      option.isDefined should be(true)
      option.get.annotations().isTracked should be(true)

      val getPost = operations.find(_.method.value().equals("post")).get
      val shape   = getPost.request.payloads.asSeq.head.schema.asInstanceOf[AnyShape]
      val option2 = shape
        .trackedExample(
          "file://amf-client/shared/src/test/resources/resolution/payloads-examples-resolution.raml#/web-api/end-points/%2Fendpoint1/get/request/application%2Fjson")
        .asOption
      option2.isDefined should be(true)
      option2.get.annotations().isTracked should be(true)

      shape.examples.asSeq
        .find(_.id.equals(
          "file://amf-client/shared/src/test/resources/resolution/payloads-examples-resolution.raml#/declarations/types/A/example/declared"))
        .head
        .annotations()
        .isTracked should be(false)
    }
  }

  test("Test accessor to double parsed field") {
    for {
      _ <- AMF.init().asFuture
      unit <- new Raml10Parser()
        .parseFileAsync("file://amf-client/shared/src/test/resources/clients/double-field.raml")
        .asFuture
    } yield {
      val shape = unit.asInstanceOf[Document].declares.asSeq.head.asInstanceOf[ScalarShape]
      shape.minimum.value() should be(1.1)
      succeed
    }
  }

  test("Test invalid mime type at lib include") {
    val include = "amf://types/Person.raml"

    val input = s"""
                   |#%RAML 1.0
                   |title: test
                   |uses:
                   |  lib: http://mylib.com
                   |types:
                   |  Person: lib.A
    """.stripMargin

    val lib = """|#%RAML 1.0 Library
                 |types:
                 |  A:
                 |    properties:
                 |      name: string
                 """.stripMargin

    import amf.client.convert.WebApiClientConverters._

    case class TestResourceLoader() extends ResourceLoader {
      override def fetch(resource: String): ClientFuture[Content] =
        Future.successful(new Content(lib, resource, Some("text/plain"))).asClient

      override def accepts(resource: String): Boolean = true
    }

    val environment = Environment
      .empty()
      .add(TestResourceLoader().asInstanceOf[ClientLoader])

    for {
      _    <- AMF.init().asFuture
      unit <- new RamlParser(environment).parseStringAsync(input).asFuture
      v    <- AMF.validate(unit, Raml10Profile, RAMLStyle).asFuture
    } yield {
      //println("report: " + v.toString)
      v.conforms should be(true)
      val declarations = unit.asInstanceOf[Document].declares.asSeq
      declarations should have size 1
    }
  }

  test("Test yaml swagger 2.0 api") {

    val environment = DefaultEnvironment()

    for {
      _ <- AMF.init().asFuture
      unit <- new Oas20YamlParser(environment)
        .parseFileAsync("file://amf-client/shared/src/test/resources/clients/oas20-yaml.yaml")
        .asFuture
    } yield {
      val location: String = unit.location
      assert(location != "")
    }
  }

  test("Test yaml swagger 2.0 api with json parser") {

    val environment = DefaultEnvironment()
    recoverToSucceededIf[UnsupportedVendorException] {
      AMF.init().asFuture.flatMap { _ =>
        new Oas20Parser(environment)
          .parseFileAsync("file://amf-client/shared/src/test/resources/clients/oas20-yaml.yaml")
          .asFuture
          .map { _ =>
            succeed
          }
      }
    }

  }

  test("Test path resolution OAS for 'file:///' prefix") {
    val file    = platform.fs.syncFile("amf-client/shared/src/test/resources/clients/toupdir-include/spec/swagger.json")
    val absPath = getAbsolutePath(file.path)
    for {
      _      <- AMF.init().asFuture
      unit   <- new Oas20Parser().parseFileAsync(absPath).asFuture
      report <- AMF.validate(unit, RamlProfile, AMFStyle).asFuture
    } yield {
      assert(report.conforms)
    }
  }

  // todo: move to common (file system)
  def getAbsolutePath(path: String): String
}
