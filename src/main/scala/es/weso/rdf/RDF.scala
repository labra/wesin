package es.weso.rdf

import es.weso.rdfgraph.statements._
import scala.util.Try
import es.weso.rdfgraph.nodes.IRI
import es.weso.rdfgraph.nodes.RDFNode

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
  
  def subjects(): Set[IRI] = {
    rdfTriples.map(_.subj).filter(_.isIRI).map(_.toIRI)
  }

  def predicates(): Set[IRI] = {
    rdfTriples.map(_.pred).filter(_.isIRI).map(_.toIRI)
  }

  def objects(): Set[IRI] = {
    rdfTriples.map(_.obj).filter(_.isIRI).map(_.toIRI)
  }
  
  def iris(): Set[IRI] = {
    rdfTriples.map(_.iris).flatten
  }

  def triplesWithSubject(n: RDFNode): Set[RDFTriple]
  
  def triplesWithObject(n: RDFNode): Set[RDFTriple]

}

object RDF {
}