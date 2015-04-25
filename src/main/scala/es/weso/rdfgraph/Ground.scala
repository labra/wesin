package es.weso.rdfgraph

import es.weso.tgraph.{ TContext, TGraph }
import es.weso.rdfgraph.nodes.{ IRI, BNodeId, RDFNode }
import es.weso.rdfgraph.statements.RDFTriple
import scala.collection.immutable.Map
import scala.collection.Set

/*
case class that provides implementation for RDF graph, it is also used in RDFGraph object factory
 */
case class Ground(graph: TGraph[RDFNode])(implicit seed: BNodeId) extends RDFGraph {

  override def isEmpty = graph.isEmpty

  override def insertTriple(triple: RDFTriple): RDFGraph = {
    Ground(graph.addTriple(triple.subj, triple.pred, triple.obj))
  }

  def getBNodeMap(node: RDFNode, map: Map[BNodeId, BNodeId]): RDFNode = {
    node match {
      case b @ BNodeId(_) => map(b)
      case other => other
    }
  }

  override def insertTripleMap(triple: RDFTriple,
    map: Map[BNodeId, BNodeId]): RDFGraph = {
    val s = getBNodeMap(triple.subj, map)
    val p = triple.pred
    val o = getBNodeMap(triple.obj, map)
    Ground(graph.addTriple(s, p, o))
  }

  override def addTriplesBNodes(
    bnodes: Set[BNodeId],
    triples: Set[RDFTriple],
    map: Map[BNodeId, BNodeId]): RDFGraph = {
    if (bnodes.isEmpty) {
      val current: RDFGraph = this
      triples.foldLeft(current)((g, triple) =>
        g.insertTripleMap(triple, map))
    } else {
      Exists {
        case (bnode) => addTriplesBNodes(bnodes.tail,
          triples,
          map + (bnodes.head -> bnode))
      }
    }
  }

  /**
   * addTriples inserts a set of triples into a graph
   *
   * It takes control of possible bnodes in the triples renaming them
   * @param triples set of triples
   *
   */
  override def addTriples(triples: Set[RDFTriple]): RDFGraph = {
    val bnodes = RDFTriple.collectBNodes(triples)
    addTriplesBNodes(bnodes, triples, Map.empty)
  }

  override def IRIs: Set[IRI] = {
    graph.nodes.filter(_.isIRI).map(_.toIRI)
  }

  /**
   * get the triples of a graph
   *
   * @param seed represents the seed for blank node identifier generation
   * (default value = 0)
   *
   * Ground graphs ignore the seed parameter
   *
   */
  override def triples(implicit seed: BNodeId): Set[RDFTriple] = {
    graph.triples.map((t) => RDFTriple(t._1, t._2.toIRI, t._3))
  }

  override def merge(other: RDFGraph): RDFGraph = {
    val g = this.addTriples(other.triples)
    g
  }

  override def show(implicit seed: BNodeId): String = {
    this.toString
  }

  /**
   * Decompose a graph from a given IRI
   * @param node resource from which we are decomposing the graph
   *
   * @author labra
   */
  def decomp(node: IRI): Option[(TContext[RDFNode], RDFGraph)] = {
    graph.decomp(node) match {
      case None => None
      case Some((ctx, g)) => Some((ctx, Ground(g)))
    }
  }

  override def foldRDFGraphSeed[A](e: A, f: (A, TContext[RDFNode]) => A, seed: BNodeId): A = {
    graph.foldTGraph(e)(f)
  }

  def foldRDFGraphSeedOrd[A](
    e: A,
    f: (A, TContext[RDFNode]) => A,
    seed: BNodeId)(implicit ord: Ordering[RDFNode]): A = {
    graph.foldTGraphOrd(e)(f)(ord)
  }

}
