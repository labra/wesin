package es.weso.wesin

import es.weso.rdfgraph._
import es.weso.utils.IO._
import com.typesafe.config._
import org.rogach.scallop._
import org.rogach.scallop.exceptions.Help
import org.slf4j.LoggerFactory
import es.weso.rdf.RDFTriples
import scala.util._

class Opts(
    arguments: Array[String],
    onError: (Throwable, Scallop) => Nothing) extends ScallopConf(arguments) {

  banner("""| Wesin
              | Options:
              |""".stripMargin)
  footer("Enjoy!")
  version("Wesin 0.1")
  val data = opt[String]("data",
    required = true,
    descr = "Data file")
  val show = toggle("show",
    prefix = "no-",
    default = Some(false),
    descrYes = "show RDF",
    descrNo = "don't show RDF")
  val verbose = toggle("verbose",
    prefix = "no-",
    default = Some(false),
    descrYes = "Normal output",
    descrNo = "Verbose output")
  val output = opt[String]("out",
    descr = "Output RDF to file")
  val version = opt[Boolean]("version",
    noshort = true,
    descr = "Print version")
  val help = opt[Boolean]("help",
    noshort = true,
    descr = "Show this message")

  override protected def onError(e: Throwable) = onError(e, builder)
}

object Wesin extends App {

  override def main(args: Array[String]) {

    val log = LoggerFactory.getLogger("Application")
    val conf = ConfigFactory.load()
    val opts = new Opts(args, errorDriver)
    val verb: Boolean = opts.verbose()

    def verbose(str: String): Try[Any] = {
      if (verb) log.debug(str)
      Success()
    }

    val dataFile = opts.data()
    val result =
      for (
        cs <- getContents(dataFile); _ <- verbose("Contents:\n" + cs); rdf <- RDFTriples.parse(cs); _ <- verbose("RDF: " + rdf)
      ) yield rdf
    result match {
      case Success(rdf) => {
        println("Parsed ok")
        if (opts.show()) {
          println(rdf)
        }
      }
      case Failure(e) => println("Exception: " + e.getMessage)
    }
  }

  private def errorDriver(e: Throwable, scallop: Scallop) = e match {
    case Help(s) =>
      println("Help: " + s)
      scallop.printHelp
      sys.exit(0)
    case _ =>
      println("Error: %s".format(e.getMessage))
      scallop.printHelp
      sys.exit(1)
  }

}

