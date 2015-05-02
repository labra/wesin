package es.weso.rdf

import es.weso.rdfgraph.statements._
import scala.util.Try
import es.weso.rdfgraph.nodes.IRI
import es.weso.rdfgraph.nodes.RDFNode

trait RDFReader {

  /**
   * parse a string and obtain an RDF graph
   */
  def parse(cs: CharSequence, format: String = "TURTLE"): Try[RDFReader]

  /**
   * convert a RDF graph to a String
   */
  def serialize(format: String = "TURTLE"): String

  /**
   * Set of RDFTriples in a graph
   */
  def rdfTriples(): Set[RDFTriple]

  /**
   * Returns the set of subjects that are IRIs in a graph
   */
  def subjects(): Set[IRI] = {
    rdfTriples.map(_.subj).filter(_.isIRI).map(_.toIRI)
  }

  /**
   * Returns the set of predicates
   */
  def predicates(): Set[IRI] = {
    rdfTriples.map(_.pred).filter(_.isIRI).map(_.toIRI)
  }

  /**
   * Returns the set of objects that are IRIs in a graph
   */
  def objects(): Set[IRI] = {
    rdfTriples.map(_.obj).filter(_.isIRI).map(_.toIRI)
  }

  /**
   * The set of all iri's available in an RDF graph
   */
  def iris(): Set[IRI] = {
    rdfTriples.map(_.iris).flatten
  }

  /**
   * Set of RDFTriples that contain a node as subject
   * @param node
   */
  def triplesWithSubject(n: RDFNode): Set[RDFTriple]

  /**
   * Set of RDFTriples that contain a node as subject
   * @param node
   */
  def triplesWithObject(n: RDFNode): Set[RDFTriple]

  /**
   * Set of RDFTriples that contain a node as subject
   * @param node
   */
  def triplesWithPredicateObject(p: IRI, o: RDFNode): Set[RDFTriple]

  /**
   * Prefix map
   */
  def getPrefixMap(): PrefixMap

}

