package es.weso.rdf

import es.weso.rdfgraph.statements._
import scala.util.Try
import es.weso.rdfgraph.nodes._
import PREFIXES._

trait RDFBuilder {

  def addPrefixMap(pm: PrefixMap): RDFBuilder

  def addPrefix(alias: String, iri: String): RDFBuilder

  def createBNode: (RDFNode, RDFBuilder)

  def addTriples(triples: Set[RDFTriple]): RDFBuilder

  def addTriple(triple: RDFTriple): RDFBuilder = {
    addTriples(Set(triple))
  }

  def addType(node: RDFNode, typeNode: RDFNode): RDFBuilder = {
    addTriple(RDFTriple(node, rdf_type, typeNode))
  }

  def rmTriple(triple: RDFTriple): RDFBuilder

  def qName(str: String): IRI

}

