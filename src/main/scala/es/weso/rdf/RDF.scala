package es.weso.rdf

import es.weso.rdfgraph.statements._
import scala.util.Try
import es.weso.rdfgraph.nodes.IRI

trait RDF {
  
  /**
   * parse a string and obtain an RDF graph
   */
  def parse(cs: CharSequence): Try[RDF]
  
  /**
   * convert a graph to a String 
   */
  def serialize(format:String = "TURTLE"): String
  
  def rdfTriples(): Set[RDFTriple]
  
  def iris(): Set[IRI] = {
    val ts = rdfTriples
    ts.map(_.iris).flatten
  }

  def triplesWithSubject(iri: IRI): Set[RDFTriple]
  
  def triplesWithObject(iri: IRI): Set[RDFTriple]

}

object RDF {
}