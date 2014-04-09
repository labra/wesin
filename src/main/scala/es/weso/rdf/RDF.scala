package es.weso.rdf

import es.weso.rdfgraph.statements._

trait RDF {
  
  /**
   * parse a string and obtain an RDF graph
   */
  def fromString(str: String)
  
  def rdfTriples(): Set[RDFTriple]
}

object RDF {
}