package amf

import amf.client.commands._
import amf.core.client.{ExitCodes, ParserConfig}
import amf.core.unsafe.PlatformSecrets

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

/**
  * Main entry point for the application
  */
object Main extends PlatformSecrets {

  def main(args: Array[String]): Unit = {
    CmdLineParser.parse(args) match {
      case Some(cfg) =>
        cfg.mode match {
          case Some(ParserConfig.REPL)      => runRepl()
          case Some(ParserConfig.TRANSLATE) => Await.result(runTranslate(cfg), 1 day)
          case Some(ParserConfig.VALIDATE)  => Await.result(runValidate(cfg), 1 day)
          case Some(ParserConfig.PARSE)     => Await.ready(runParse(cfg), 1 day)
          case Some(ParserConfig.PATCH)     => Await.ready(runPatch(cfg), 1 day)
          case _                            => failCommand()
        }
      case _ => System.exit(ExitCodes.WrongInvocation)
    }
    System.exit(ExitCodes.Success)
  }

  def failCommand(): Unit = {
    System.err.println("Wrong command")
    System.exit(ExitCodes.WrongInvocation)
  }
  def runRepl()                                       = Repl(System.in, System.out)
  def runTranslate(config: ParserConfig): Future[Any] = TranslateCommand(platform).run(config)
  def runValidate(config: ParserConfig): Future[Any]  = ValidateCommand(platform).run(config)
  def runParse(config: ParserConfig): Future[Any]     = ParseCommand(platform).run(config)
  def runPatch(config: ParserConfig): Future[Any]     = PatchCommand(platform).run(config)
}
