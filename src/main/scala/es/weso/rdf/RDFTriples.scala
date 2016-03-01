package es.weso.rdf

/** Implementation of RDF trait using Sets of triples **/
import es.weso.rdf.triples._
// import es.weso.rdf.RDFGraph
import es.weso.parser.TurtleParser
import scala.util._
import es.weso.rdf.nodes.InitialBNodeId
import es.weso.rdf.nodes.IRI
import es.weso.rdf.nodes.RDFNode

case class RDFTriples(
    triples: Set[RDFTriple],
    pm: PrefixMap
) extends RDFReader {
  type Rdf = RDFTriples

  override def parse(cs: CharSequence, format: String, base: Option[String]): Try[Rdf] = {
    val baseURI = IRI(base.getOrElse(""))
    format match {
      case "TURTLE" => for ((triples, pm) <- TurtleParser.parse(cs, baseURI))
        yield RDFTriples(triples, pm)
      case _ => Failure(throw new Exception("Unsopported format: " + format))
    }
  }

  override def serialize(format: String): String = {
    format match {
      case "TURTLE" => {
        val sb = new StringBuilder
        for (t <- triples) {
          sb ++= t.toString() + "\n"
        }
        sb.toString
      }
      case _ => throw new Exception("Format: " + format + " not handled to serialize RDF triples")
    }
  }

  override def rdfTriples: Set[RDFTriple] = triples

  override def triplesWithSubject(node: RDFNode): Set[RDFTriple] = {
    triples.filter(_.hasSubject(node))
  }

  override def triplesWithObject(node: RDFNode): Set[RDFTriple] = {
    triples.filter(_.hasObject(node))
  }

  override def triplesWithPredicate(p: IRI): Set[RDFTriple] = {
    triples.filter(_.hasPredicate(p))
  }

  override def triplesWithPredicateObject(p: IRI, o: RDFNode): Set[RDFTriple] = {
    triples.filter(_.hasObject(o)).filter(_.hasPredicate(p))
  }

  override def getPrefixMap: PrefixMap = pm
}

object RDFTriples {

  def noTriples: RDFReader = RDFTriples(Set(), PrefixMap.empty)

  def parse(cs: CharSequence): Try[RDFReader] =
    noTriples.parse(cs)

}