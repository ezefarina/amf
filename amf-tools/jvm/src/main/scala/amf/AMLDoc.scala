package amf

import java.io.File
import java.net.URLEncoder

import amf.core.model.document.BaseUnit
import amf.core.rdf.RdfModel
import amf.core.remote.Context
import amf.core.services.RuntimeCompiler
import amf.core.unsafe.PlatformSecrets
import amf.plugins.document.vocabularies.AMLPlugin
import amf.plugins.document.vocabularies.model.document.Vocabulary
import amf.plugins.features.validation.AMFValidatorPlugin
import org.apache.jena.query.{QueryExecutionFactory, QueryFactory, QuerySolution}
import org.apache.jena.rdf.model.{Model, ModelFactory}
import scalatags.Text
import scalatags.Text.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

case class NonExistingVocabularyFile(path: String) extends Exception

case class ParsedExternalFramgent(unit: BaseUnit) extends Exception

case class VocabularyInfo(location: String, base: String, usage: Option[String] = None, references: Seq[String] = Nil, externals: Seq[String] = Nil, classTerms: Seq[String] = Nil, literalProperties: Seq[String] = Nil, objectProperties: Seq[String] = Nil) {
  def name: Text.Modifier = location.split("/").last
}

object AMLDoc extends PlatformSecrets {

  def AMFInit(): Future[Unit] = {
    amf.core.AMF.registerPlugin(AMLPlugin)
    amf.core.AMF.registerPlugin(AMFValidatorPlugin)
    amf.core.AMF.init()

  }


  def generate(directoryOrFile: String) = {
    for {
      _            <- AMFInit()
      models       <- parseAllFiles(vocabularyFiles(directoryOrFile))
      model: Model <- loadModels(models)
    }  yield {
      println("*** ALL PARSED")
      /*
      val stmts = model.listStatements()
      while(stmts.hasNext) {
        println(stmts.next())
      }
      */

      /*
      listVocabularies(model).foreach { info =>
        println(info)
      }
      */
      println(render(listVocabularies(model)))
    }
  }

  protected def vocabularyFiles(directoryOrFile: String): Seq[File] = {
    val f = new File(directoryOrFile)
    if (f.exists()) {
      if (f.isDirectory) {
        f.listFiles().filter(_.isFile)
      } else {
        Seq(f)
      }
    } else {
      throw NonExistingVocabularyFile(f.getAbsolutePath)
    }
  }

  protected def parseAllFiles(files: Seq[File], acc: Seq[RdfModel] = Nil): Future[Seq[RdfModel]] = {
    if (files.isEmpty) {
      Future(acc)
    } else {
      parseAmlVocabulary(files.head) flatMap { parsed: RdfModel =>
        parseAllFiles(files.tail, acc :+ parsed)
      }
    }
  }

  protected def parseAmlVocabulary(file: File): Future[RdfModel] = {
    println(s"*** PARSING ${file.getAbsolutePath}")
    var inputFile = ensureUrl(file.getAbsolutePath)
    RuntimeCompiler(
      inputFile,
      Some("application/yaml"),
      Some("AML 1.0"),
      Context(platform)
    ) map {
      case unit: Vocabulary =>
        println(s"*** PARSED VOCABULARY ${file.getAbsolutePath}")
        unit.toNativeRdfModel()
      case unit             =>
        println(s"*** PARSED EXTERNAL ${file.getAbsolutePath}")
        throw ParsedExternalFramgent(unit)
    }
  }

  protected def ensureUrl(inputFile: String): String =
    if (!inputFile.startsWith("file:") && !inputFile.startsWith("http:") && !inputFile.startsWith("https:")) {
      s"file://${inputFile}"
    } else {
      inputFile
    }

  protected def loadModels(models: Seq[RdfModel]): Future[Model] = {
    println("*** LOADING MODELS")
    val model = ModelFactory.createDefaultModel()

    models.foreach { parsed =>
      model.add(parsed.native().asInstanceOf[Model])
    }

    Future(model)
  }

  protected def listVocabularies(model: Model): Seq[VocabularyInfo] = {
    val queryString =
      """SELECT * {
        |  ?vocab a <http://a.ml/vocabularies/meta#Vocabulary> ;
        |         ?p ?o
        |} ORDER BY ?vocab
      """.stripMargin

    query(model, queryString).groupBy(_.get("vocab").toString).map { case (v, solutions) =>
      println(s"  VOCAB ${v}")
      var vocab = VocabularyInfo(location = "", base = "")
      solutions.foreach { sol =>
        sol.get("p").toString match {
          case "http://a.ml/vocabularies/document#location"  =>
            vocab = vocab.copy(location = sol.get("o").toString)
          case "http://a.ml/vocabularies/document#usage"  =>
            vocab = vocab.copy(usage = Some(sol.get("o").toString))
          case "http://a.ml/vocabularies/meta#base"       =>
            vocab = vocab.copy(base = sol.get("o").toString)
          case "http://a.ml/vocabularies/meta#externals"  =>
            val external = sol.get("o").toString
            val externalBase = query(model, s"SELECT * { <$external> <http://a.ml/vocabularies/meta#base> ?o }").head.get("o").toString
            vocab = vocab.copy(externals = vocab.externals :+ externalBase)
          case "http://a.ml/vocabularies/document#references"  =>
            val oldRefs = vocab.references
            vocab = vocab.copy(references = oldRefs :+ sol.get("o").toString)
          case "http://a.ml/vocabularies/document#declares"    =>
            val declaration = sol.get("o").toString
            query(model, s"SELECT * { <${declaration}> a ?type }").filter(_.get("type").toString != "http://a.ml/vocabularies/meta#Property").head.get("type").toString match {
              case "http://www.w3.org/2002/07/owl#DatatypeProperty" =>
                val oldLiteralProps = vocab.literalProperties
                vocab = vocab.copy(literalProperties = oldLiteralProps :+ declaration)
              case "http://www.w3.org/2002/07/owl#ObjectProperty"   =>
                val oldObjProps = vocab.objectProperties
                vocab = vocab.copy(objectProperties = oldObjProps :+ declaration)
              case "http://www.w3.org/2002/07/owl#Class"            =>
                val oldClassTerms = vocab.classTerms
                vocab = vocab.copy(classTerms = oldClassTerms :+ declaration)
              case _                                                => // ignore
            }
          case _ => // ignore
        }
      }

      vocab
    } toSeq
  }


  protected def query(jenaModel: Model, queryString: String): Seq[QuerySolution] = {
    // println(s"*** RUNNING QUERY ${queryString}")
    val query       = QueryFactory.create(queryString)
    val qexec       = QueryExecutionFactory.create(query, jenaModel)
    try {
      var acc: Seq[QuerySolution] = Seq()
      val results = qexec.execSelect
      while (results.hasNext) {
        val soln = results.nextSolution
        acc = acc :+ soln
      }
      acc
    } finally {
      if (qexec != null) qexec.close()
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"*** GENERATING ${args(0)}")
    generate(args(0)) onComplete {
      case Success(_) =>
        println("*** FINISHED!")
      case Failure(e) =>
        println(s"*** ERROR ${e}")
    }
  }

  protected def render(vocabularies: Seq[VocabularyInfo]) = {
    html(
      head(),
      body(
        renderVocabulariesPanel(vocabularies)
      )
    )
  }

  protected def renderVocabulariesPanel(vocabularies: Seq[VocabularyInfo]): Text.TypedTag[String] = {
    div(id := "vocabularies",
      h1("Vocabularies"),
      ul(
        vocabularies.map { v =>
          li(
            a(id := encodeUrl(v.base),v.name)
          )
        }
      )
    )
  }

  protected def encodeUrl(text: String) = URLEncoder.encode(text, "UTF-8")
}
