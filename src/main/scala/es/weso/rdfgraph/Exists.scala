package es.weso.rdfgraph

import es.weso.rdf.nodes.{ RDFNode, IRI, BNodeId }
import es.weso.rdf.triples.RDFTriple
import scala.collection.Set

import scala.collection.immutable.Map
import es.weso.tgraph.TContext

case class Exists(fn: BNodeId => RDFGraph)(implicit seed: BNodeId) extends RDFGraph {

  override def isEmpty = false

  override def insertTriple(triple: RDFTriple): RDFGraph = {
    Exists { case (bnode) => fn(bnode).insertTriple(triple) }
    //Anton: I had to add "case" because of IntellijIDEA highlightning bug that I discovered http://youtrack.jetbrains.com/issue/SCL-6730
  }

  override def addTriples(triples: Set[RDFTriple]): RDFGraph = {
    Exists { case (bnode) => fn(bnode).addTriples(triples) }
  }

  override def triples(implicit seed: BNodeId): Set[RDFTriple] = {
    fn(seed).triples(seed.newBNodeId)
  }

  override def IRIs: Set[IRI] = {
    fn(seed).IRIs
  }

  /*
   * merges this graph with another one
   */
  override def merge(other: RDFGraph): RDFGraph = {
    Exists { case bnode => fn(bnode).merge(other) }
  }

  /*
   * add triples which can have a set of bNodes
   */
  override def addTriplesBNodes(
    bnodes: Set[BNodeId],
    triples: Set[RDFTriple],
    map: Map[BNodeId, BNodeId]
  ): RDFGraph = {
    Exists { case (bnode) => fn(bnode).addTriplesBNodes(bnodes, triples, map) }
  }

  override def insertTripleMap(
    triple: RDFTriple,
    map: Map[BNodeId, BNodeId]
  ): RDFGraph = {
    Exists { case (bnode) => fn(bnode).insertTripleMap(triple, map) }
  }

  override def show(implicit seed: BNodeId): String = {
    "Exists " + seed.id + " ( " + fn(seed).show(seed.newBNodeId) + ")"
  }

  override def foldRDFGraphSeed[A](e: A, f: (A, TContext[RDFNode]) => A, seed: BNodeId): A = {
    fn(seed).foldRDFGraphSeed(e, f, seed.newBNodeId)
  }

  def foldRDFGraphSeedOrd[A](
    e: A,
    f: (A, TContext[RDFNode]) => A,
    seed: BNodeId
  )(implicit ord: Ordering[RDFNode]): A = {
    fn(seed).foldRDFGraphSeedOrd(e, f, seed.newBNodeId)(ord)
  }

}