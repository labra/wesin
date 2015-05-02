package es.weso.rdf

import es.weso.rdfgraph.statements._
import scala.util.Try
import es.weso.rdfgraph.nodes.IRI
import es.weso.rdfgraph.nodes.RDFNode

trait RDFBuilder {

  def addPrefixMap(pm: PrefixMap): RDFBuilder

  def createBNode: RDFNode

  def addTriples(triples: Set[RDFTriple]): RDFBuilder

  def rmTriple(triple: RDFTriple): RDFBuilder

}

