package amf.plugins.features.validation

import amf._
import amf.client.plugins.{AMFDocumentPlugin, AMFPlugin, AMFValidationPlugin}
import amf.core.annotations.SourceVendor
import amf.core.benchmark.ExecutionLog
import amf.core.model.document.{BaseUnit, Document, Fragment, Module}
import amf.core.rdf.RdfModel
import amf.core.registries.AMFPluginsRegistry
import amf.core.remote._
import amf.core.services.{RuntimeCompiler, RuntimeValidator, ValidationOptions}
import amf.core.unsafe.PlatformSecrets
import amf.core.validation.core.{ValidationProfile, ValidationReport, ValidationSpecification}
import amf.core.validation.{AMFValidationReport, EffectiveValidations}
import amf.internal.environment.Environment
import amf.plugins.document.graph.AMFGraphPlugin
import amf.plugins.document.vocabularies.AMLPlugin
import amf.plugins.document.vocabularies.model.document.DialectInstance
import amf.plugins.document.vocabularies.model.domain.DialectDomainElement
import amf.plugins.features.validation.emitters.{JSLibraryEmitter, ValidationJSONLDEmitter}
import amf.plugins.features.validation.model.{ParsedValidationProfile, ValidationDialectText}
import amf.plugins.syntax.SYamlSyntaxPlugin
import org.yaml.model.YDocument.PartBuilder
import org.yaml.model.YType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AMFValidatorPlugin extends ParserSideValidationPlugin with PlatformSecrets {

  override val ID = "AMF Validation"

  override def init(): Future[AMFPlugin] = {
    // Registering ourselves as the runtime validator
    RuntimeValidator.register(AMFValidatorPlugin)
    ExecutionLog.log("Register RDF framework")
    platform.rdfFramework = Some(PlatformValidator.instance)
    ExecutionLog.log(s"AMFValidatorPlugin#init: registering validation dialect")
    AMLPlugin.registry.registerDialect(url, ValidationDialectText.text) map { _ =>
      ExecutionLog.log(s"AMFValidatorPlugin#init: validation dialect registered")
      this
    }
  }

  override def dependencies() = Seq(SYamlSyntaxPlugin, AMLPlugin, AMFGraphPlugin)

  val url = "http://a.ml/dialects/profile.raml"

  // All the profiles are collected here, plugins can generate their own profiles
  def profiles: Map[String, () => ValidationProfile] =
    AMFPluginsRegistry.documentPlugins.foldLeft(Map[String, () => ValidationProfile]()) {
      case (acc, domainPlugin: AMFValidationPlugin) => acc ++ domainPlugin.domainValidationProfiles(platform)
      case (acc, _)                                 => acc
    } ++ customValidationProfiles

  // Mapping from profile to domain plugin
  def profilesPlugins: Map[String, AMFDocumentPlugin] =
    AMFPluginsRegistry.documentPlugins.foldLeft(Map[String, AMFDocumentPlugin]()) {
      case (acc, domainPlugin: AMFValidationPlugin) =>
        val toPut = domainPlugin.domainValidationProfiles(platform).keys.foldLeft(Map[String, AMFDocumentPlugin]()) {
          case (accProfiles, profileName) =>
            accProfiles.updated(profileName, domainPlugin)
        }
        acc ++ toPut
      case (acc, _) => acc
    } ++ customValidationProfilesPlugins

  var customValidationProfiles: Map[String, () => ValidationProfile]  = Map.empty
  var customValidationProfilesPlugins: Map[String, AMFDocumentPlugin] = Map.empty

  override def loadValidationProfile(validationProfilePath: String,
                                     env: Environment = Environment()): Future[ProfileName] = {
    RuntimeCompiler(
      validationProfilePath,
      Some("application/yaml"),
      Some(AMLPlugin.ID),
      Context(platform),
      cache = Cache(),
      env = env
    ).map {
        case parsed: DialectInstance if parsed.definedBy().is(url) =>
          parsed.encodes
        case _ =>
          throw new Exception(
            "Trying to load as a validation profile that does not match the Validation Profile dialect")
      }
      .map {
        case encoded: DialectDomainElement if encoded.definedBy.name.is("profileNode") =>
          val profile = ParsedValidationProfile(encoded)
          val domainPlugin = profilesPlugins.get(profile.name.profile) match {
            case Some(plugin) => plugin
            case None =>
              profilesPlugins.get(profile.baseProfile.getOrElse(AmfProfile).profile) match {
                case Some(plugin) =>
                  plugin
                case None => AMLPlugin

              }
          }
          customValidationProfiles += (profile.name.profile -> { () =>
            profile
          })
          customValidationProfilesPlugins += (profile.name.profile -> domainPlugin)
          profile.name

        case other =>
          throw new Exception(
            "Trying to load as a validation profile that does not match the Validation Profile dialect")
      }
  }

  def computeValidations(profileName: ProfileName,
                         computed: EffectiveValidations = new EffectiveValidations()): EffectiveValidations = {
    val maybeProfile = profiles.get(profileName.profile) match {
      case Some(profileGenerator) => Some(profileGenerator())
      case _                      => None
    }

    maybeProfile match {
      case Some(foundProfile) =>
        if (foundProfile.baseProfile.isDefined) {
          computeValidations(foundProfile.baseProfile.get, computed).someEffective(foundProfile)
        } else {
          computed.someEffective(foundProfile)
        }
      case None => computed
    }
  }

  override def shaclValidation(model: BaseUnit,
                               validations: EffectiveValidations,
                               options: ValidationOptions): Future[ValidationReport] =
    if (options.isPartialValidation()) partialShaclValidation(model, validations, options)
    else fullShaclValidation(model, validations, options)

  def partialShaclValidation(model: BaseUnit,
                             validations: EffectiveValidations,
                             options: ValidationOptions): Future[ValidationReport] =
    new CustomShaclValidator(model, validations, options).run

  def fullShaclValidation(model: BaseUnit,
                          validations: EffectiveValidations,
                          options: ValidationOptions): Future[ValidationReport] = {
    ExecutionLog.log(
      s"AMFValidatorPlugin#shaclValidation: shacl validation for ${validations.effective.values.size} validations")
    // println(s"VALIDATIONS: ${validations.effective.values.size} / ${validations.all.values.size} => $profileName")
    // validations.effective.keys.foreach(v => println(s" - $v"))

    // TODO: Check the validation profile passed to JSLibraryEmitter, it contains the prefixes
    // for the functions
    val jsLibrary = new JSLibraryEmitter(None).emitJS(validations.effective.values.toSeq)

    jsLibrary match {
      case Some(code) => PlatformValidator.instance.registerLibrary(ValidationJSONLDEmitter.validationLibraryUrl, code)
      case _          => // ignore
    }

    ExecutionLog.log(s"AMFValidatorPlugin#shaclValidation: jsLibrary generated")

    val data   = model
    val shapes = customValidations(validations)

    ExecutionLog.log(s"AMFValidatorPlugin#shaclValidation: Invoking platform validation")

//    ValidationMutex.synchronized {
    PlatformValidator.instance.report(data, shapes, options).map {
      case report =>
        ExecutionLog.log(s"AMFValidatorPlugin#shaclValidation: validation finished")
        report
    }
//    }
  }

  private def profileForUnit(unit: BaseUnit, given: ProfileName): ProfileName = {
    given match {
      case OasProfile =>
        getSource(unit) match {
          case Some(Oas30) => Oas30Profile
          case _           => Oas20Profile
        }
      case RamlProfile =>
        getSource(unit) match {
          case Some(Raml08) => Raml08Profile
          case _            => Raml10Profile
        }
      case _ => given
    }

  }

  private def getSource(unit: BaseUnit): Option[Vendor] = unit match {
    case d: Document => d.encodes.annotations.find(classOf[SourceVendor]).map(_.vendor)
    case m: Module   => m.annotations.find(classOf[SourceVendor]).map(_.vendor)
    case f: Fragment => f.encodes.annotations.find(classOf[SourceVendor]).map(_.vendor)
    case _           => None
  }

  override def validate(model: BaseUnit,
                        given: ProfileName,
                        messageStyle: MessageStyle,
                        env: Environment): Future[AMFValidationReport] = {

    val profileName = profileForUnit(model, given)
    super.validate(model, profileName, messageStyle, env) flatMap {
      case parseSideValidation if !parseSideValidation.conforms => Future.successful(parseSideValidation)
      case _                                                    => modelValidation(model, profileName, messageStyle, env)
    }

  }

  private def modelValidation(model: BaseUnit,
                              profileName: ProfileName,
                              messageStyle: MessageStyle,
                              env: Environment): Future[AMFValidationReport] = {

    profilesPlugins.get(profileName.profile) match {
      case Some(domainPlugin: AMFValidationPlugin) =>
        val validations = computeValidations(profileName)
        domainPlugin
          .validationRequest(model, profileName, validations, platform, env)
      case _ =>
        Future {
          profileNotFoundWarningReport(model, profileName)
        }
    }
  }

  def profileNotFoundWarningReport(model: BaseUnit, profileName: ProfileName): AMFValidationReport = {
    AMFValidationReport(conforms = true, model.location().getOrElse(model.id), profileName, Seq())
  }

  /**
    * Generates a JSON-LD graph with the SHACL shapes for the requested profile validations
    * @return JSON-LD graph
    */
  def shapesGraph(validations: EffectiveValidations, messageStyle: MessageStyle = RAMLStyle): String = {
    new ValidationJSONLDEmitter(messageStyle.profileName).emitJSON(customValidations(validations))
  }

  def customValidations(validations: EffectiveValidations): Seq[ValidationSpecification] =
    validations.effective.values.toSeq.filter(s => !s.isParserSide)

  /**
    * Returns a native RDF model with the SHACL shapes graph
    */
  override def shaclModel(validations: Seq[ValidationSpecification],
                          functionUrls: String,
                          messageStyle: MessageStyle): RdfModel =
    PlatformValidator.instance.shapes(validations, functionUrls)

}

object ValidationMutex {}
