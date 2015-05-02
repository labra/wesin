package es.weso.rdf

/** Implementation of RDF trait using Sets of triples **/
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph.RDFGraph
import es.weso.parser.TurtleParser
import scala.util._
import es.weso.rdfgraph.nodes.InitialBNodeId
import es.weso.rdfgraph.nodes.IRI
import es.weso.rdfgraph.nodes.RDFNode

case class RDFTriples(
    triples: Set[RDFTriple],
    pm: PrefixMap) extends RDFReader {

  override def parse(cs: CharSequence, format: String): Try[RDFReader] = {
    format match {
      case "TURTLE" => for ((triples, pm) <- TurtleParser.parse(cs))
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