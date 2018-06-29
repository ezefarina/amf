package amf.client.commands

import amf.core.client.{ExitCodes, ParserConfig}
import amf.core.model.document.BaseUnit
import amf.core.remote.Platform
import amf.core.services.RuntimeValidator

import scala.concurrent.Future
import scala.util.{Failure, Success}

class TranslateCommand(override val platform: Platform) extends CommandHelper {

  val validationCommand = new ValidateCommand(platform)

  def run(config: ParserConfig): Future[Any] = {
    val res: Future[Any] = for {
      _         <- AMFInit()
      model     <- parseInput(config)
      _         <- checkValidation(config, model)
      generated <- generateOutput(config, model)
    } yield {
      generated
    }

    res.onComplete {
      case Failure(ex: Throwable) => {
        config.stderr.print(ex)
        config.proc.exit(ExitCodes.Exception)
      }
      case Success(other) => other
    }
    res
  }

  def checkValidation(config: ParserConfig, model: BaseUnit): Future[Unit] = {
    val customProfileLoaded = if (config.customProfile.isDefined) {
      RuntimeValidator.loadValidationProfile(config.customProfile.get) map { profileName =>
        profileName
      }
    } else {
      Future {
        config.profile
      }
    }
    customProfileLoaded map { profileName =>
      RuntimeValidator(model, profileName) map { report =>
        if (!report.conforms) {
          config.stderr.print(report.toString)
          config.proc.exit(ExitCodes.FailingValidation)
        }
      }
    }
  }
}

object TranslateCommand {
  def apply(platform: Platform) = new TranslateCommand(platform)
}
