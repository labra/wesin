package es.weso.wesin


import es.weso.rdfgraph._
import es.weso.utils.IO._
import com.typesafe.config._
import org.rogach.scallop._
import org.rogach.scallop.exceptions.Help
import org.slf4j.LoggerFactory
import es.weso.rdf.RDFTriples

class Opts(
    arguments: Array[String],
    onError: (Throwable, Scallop) => Nothing
    ) extends ScallopConf(arguments) {

    banner("""| Wesin
              | Options:
              |""".stripMargin)
    footer("Enjoy!")
    version("Wesin 0.1")
    val data 	= opt[String]("data",
    				required=true,
    				descr = "Data file")
    val show = toggle("show", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "show RDF", 
        			descrNo = "don't show RDF")
    val verbose    = toggle("verbose", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "Normal output", 
        			descrNo = "Verbose output")
    val output  = opt[String]("out",
    				descr = "Output RDF to file")
    val version = opt[Boolean]("version", 
    				noshort = true, 
    				descr = "Print version")
    val help 	= opt[Boolean]("help", 
    				noshort = true, 
    				descr = "Show this message")
  
  override protected def onError(e: Throwable) = onError(e, builder)
}

object Wesin extends App {


 override def main(args: Array[String]) {

  val log 		= LoggerFactory.getLogger("Application")
  val conf 		= ConfigFactory.load()
  val opts 		= new Opts(args,errorDriver)

  val dataFile = opts.data()
  for (cs <- getContents(dataFile); 
      rdf <- RDFTriples.parse(cs) 
  ) {
    if (opts.show()) {
      println(rdf.toString())
    }
/*    val dataFile = opts.data()
    if (dataFile != null) {
      for (cs <- getContents(dataFile); 
          (data,prefixMapData) <- RDF.fromString(cs)) {
        if (opts.showData()) {
          println(data.toString())
        }
        
      }  
    } */
  }
/*  getContents(schemaFile) match {
    case Failure(e) => 
      println("Exception parsing file " + schemaFile + ": " + e.getLocalizedMessage())
    case Success(cs) => {
      log.info("Input string:\n" + cs)
      Schema.fromString(cs) match {
        case Success((schema,prefixMap)) => 
           if (opts.showSchema()) {
        	 log.info("Schema parsed")
        	 println(schema.toString())
    	   }
        case Failure(e) => println("Exception parsing:" + e)
      }
    }
   } */
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


