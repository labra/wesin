package es.weso.rdfgraph

import es.weso.tgraph._
import es.weso.tgraph.TGraph
import es.weso.tgraph.TGraphImpl
import scala.collection.Set
import scala.collection.immutable.{ Map }
import scalax.collection.immutable.Graph
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes._

/**
 * Type of exceptions raised
 */
case class RDFGraphException(msg: String) extends Exception

abstract class RDFGraph {

  def isEmpty: Boolean

  /**
   * insertTriple inserts a triple into a graph
   * Blank nodes inside the triple may conflict with graph blank nodes
   */
  def insertTriple(triple: RDFTriple): RDFGraph

  /**
   * addTriples adds a set of triples
   *
   * Blank nodes in the set of triples are renamed
   *
   */
  def addTriples(triples: Set[RDFTriple]): RDFGraph

  /**
   * triples returns the set of triples of a graph
   *
   * @param seed Initial BNode identifier to start renaming
   */
  def triples(implicit seed: BNodeId): Set[RDFTriple]

  /**
   * set of IRIs of this RDFGraph
   */
  def IRIs: Set[IRI]

  /**
   * Merges this graph with a new one
   */
  def merge(other: RDFGraph): RDFGraph

  /**
   * addTriplesBNodes adds triples to this graph
   *
   * @param triples set of triples to insert
   * @param map maps from BNodeIds in triples to BNodeIds in graph
   */
  def addTriplesBNodes(
    bnodes: Set[BNodeId],
    triples: Set[RDFTriple],
    map: Map[BNodeId, BNodeId]
  ): RDFGraph

  /**
   * insertTripleMap inserts a triple in this graph
   *
   * @param triple to insert
   * @param map maps from BNodeIds in triple to BNodeIds in Graph
   *
   */
  def insertTripleMap(
    triple: RDFTriple,
    map: Map[BNodeId, BNodeId]
  ): RDFGraph

  def show(implicit seed: BNodeId): String

  def foldRDFGraph[A](
    e: A,
    f: (A, TContext[RDFNode]) => A
  )(implicit seed: BNodeId): A = {
    foldRDFGraphSeed(e, f, seed)
  }

  def foldRDFGraphSeed[A](
    e: A,
    fn: (A, TContext[RDFNode]) => A,
    seed: BNodeId
  ): A

  def foldRDFGraphOrd[A](
    e: A,
    f: (A, TContext[RDFNode]) => A
  )(implicit
    ord: Ordering[RDFNode],
    seed: BNodeId): A = {
    foldRDFGraphSeedOrd(e, f, seed)(ord)
  }

  def foldRDFGraphSeedOrd[A](
    e: A,
    fn: (A, TContext[RDFNode]) => A,
    seed: BNodeId
  )(implicit ord: Ordering[RDFNode]): A

  //  def isomorphic(other : RDFGraph) : Boolean

}

object RDFGraph {

  // Implicit definition for starting BNodeId
  // Should I put it in other place?
  implicit val initialBNode = BNodeId("b" + 0)

  /**
   * Empty RDF Graph (no nodes and edges)
   */
  def empty: RDFGraph = Ground(new TGraphImpl(Graph[RDFNode, Triple]()))

  /**
   * Context represents the context of an IRI in a RDF Graph
   *
   * @param iri iri from which this context is about
   * @param succ list of succesors (outgoing edges): pairs of (property,object)
   * @param pred list of predecessors (incoming edges): pairs of (subject,property)
   *
   */
  def showFolds(g: RDFGraph): String = {
    g.foldRDFGraph(
      "\n",
      (r: String, ctx: TContext[RDFNode]) => "ctx: " + ctx + "\n" + r
    )
  }

  implicit def minOrd = new Ordering[IRI] {
    def compare(a: IRI, b: IRI) = b.str compare a.str
  }

  def showFoldsOrd(g: RDFGraph): String = {
    implicit val ord = new Ordering[RDFNode] {
      def compare(x: RDFNode, y: RDFNode): Int =
        (x, y) match {
          case (a: IRI, b: IRI) => a.uri.compareTo(b.uri)
          case (_: IRI, BNodeId(_)) => -1
          case (_: IRI, _: Literal) => -1
          case (_: BNodeId, _: IRI) => 1
          case (_: Literal, _: IRI) => 1
          case (BNodeId(a), BNodeId(b)) => a.compare(b)
          case (l1: Literal, l2: Literal) => 0 // TODO: Maybe we should compare literals by their lexical form
          case (BNodeId(_), l: Literal) => 1 // TODO: Comparing literals and BNodes could be irrelevant...maybe 0 ?
          case (l: Literal, BNodeId(_)) => -1
          case _ => throw new RDFNodeException("Unexpected values " + (x, y) + " comparing RDFNodes")
        }
    }
    g.foldRDFGraphOrd("", (r: String, ctx: TContext[RDFNode]) => "ctx: " + ctx + "\n" + r)
  }

  def fromTriples(triples: Set[RDFTriple]): RDFGraph = {
    empty.addTriples(triples)
  }

}
