package amf.tools.canonical
import amf.core.AMFCompiler
import amf.core.registries.AMFPluginsRegistry
import amf.core.unsafe.PlatformSecrets
import amf.core.validation.AMFValidationReport
import amf.facades.Validation
import amf.plugins.document.graph.AMFGraphPlugin
import amf.plugins.features.validation.{AMFValidatorPlugin, ParserSideValidationPlugin, PlatformValidator}
import amf.plugins.syntax.SYamlSyntaxPlugin
import org.scalatest.{AsyncFunSuite, FunSuite}

import scala.concurrent.Future

class TestForJenaSystemInit extends AsyncFunSuite with PlatformSecrets {

  test(" try to init bad") {

    AMFValidatorPlugin.init().map(_ => succeed)

  }

  test(" try to init etc") {

    Future {
      val op = Some(PlatformValidator.instance)
      succeed
    }

  }

}
