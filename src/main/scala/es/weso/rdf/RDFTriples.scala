package es.weso.rdf

/** Implementation of RDF trait using Sets of triples **/
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph.RDFGraph
import es.weso.parser.PrefixMap
import es.weso.parser.TurtleParser
import scala.util.Try
import es.weso.rdfgraph.nodes.InitialBNodeId
import es.weso.rdfgraph.nodes.IRI

case class RDFTriples(
    triples: Set[RDFTriple], 
    pm: PrefixMap) extends RDF {
 
 override def parse(cs: CharSequence): Try[RDF] = {
   for ((triples,pm) <- TurtleParser.parse(cs)) 
      yield RDFTriples(triples,pm)
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
 
 override def rdfTriples : Set[RDFTriple] = triples
 
 override def triplesWithSubject(iri: IRI) : Set[RDFTriple] = {
   triples.filter(t => t.subj.toIRI == iri)
 }
   
 override def triplesWithObject(iri: IRI) : Set[RDFTriple] = {
   triples.filter(t => t.obj.isIRI && t.obj.toIRI == iri)
 }

}

object RDFTriples {

  def noTriples : RDF = RDFTriples(Set(),PrefixMap.empty)
  
  def parse(cs:CharSequence): Try[RDF] =
    noTriples.parse(cs)

}