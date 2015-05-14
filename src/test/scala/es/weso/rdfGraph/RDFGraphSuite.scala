package es.weso.rdfGraph

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import es.weso.rdfgraph.nodes._
import es.weso.tgraph.TContext
import es.weso.rdfgraph.RDFGraph
import es.weso.rdfgraph.statements.RDFTriple

@RunWith(classOf[JUnitRunner])
class RDFGraphSuite extends FunSuite {

  implicit val initialBnode: BNodeId = BNodeId("b" + 0)

  test("empty graph") {
    val g = RDFGraph.empty
    assert(g.isEmpty, true)
  }

  test("graph with one basic triple") {
    val t = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
    val g = RDFGraph.empty
    val g1 = g.insertTriple(t)
    assert(g1.triples === Set(t))
  }

  test("graph with two basic triples") {
    val t1 = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
    val t2 = RDFTriple(IRI("a"), IRI("b"), IRI("d"))
    val g = RDFGraph.empty
    val g1 = g.addTriples(Set(t1, t2))
    assert(g1.triples === Set(t1, t2))
  }

  test("collect bNodes of a set of triples") {
    val t1 = RDFTriple(BNodeId("b" + 1), IRI("b"), IRI("c"))
    val t2 = RDFTriple(IRI("a"), IRI("b"), BNodeId("b" + 2))
    val t3 = RDFTriple(IRI("a"), IRI("b"), BNodeId("b" + 3))
    val t4 = RDFTriple(BNodeId("b" + 2), IRI("b"), BNodeId("b" + 4))
    val s = Set(t1, t2, t3, t4)
    assert(RDFTriple.collectBNodes(s) ===
      Set(BNodeId("b" + 1), BNodeId("b" + 2), BNodeId("b" + 3), BNodeId("b" + 4)))
  }

  test("insert a triple with a bnode in subject") {
    val t1 = RDFTriple(BNodeId("b" + 0), IRI("b"), IRI("c"))
    val g = RDFGraph.empty.addTriples(Set(t1))
    assert(g.triples(BNodeId("b" + 0)) === Set(t1))
  }

  test("insert a triple with a bnode in object") {
    val t1 = RDFTriple(IRI("a"), IRI("b"), BNodeId("b" + 0))
    val g = RDFGraph.empty.addTriples(Set(t1))
    assert(g.triples(BNodeId("b" + 0)) === Set(t1))
  }

  test("insert a triple with two bnodes identical") {
    val t1 = RDFTriple(BNodeId("b" + 0), IRI("b"), BNodeId("b" + 0))
    val g = RDFGraph.empty.addTriples(Set(t1))
    assert(g.triples(BNodeId("b" + 0)) === Set(t1))
  }

  test("insert a triple with two bnodes different") {
    val t1 = RDFTriple(BNodeId("b" + 0), IRI("b"), BNodeId("b" + 1))
    val g = RDFGraph.empty.addTriples(Set(t1))
    assert(g.triples(BNodeId("b" + 0)) === Set(t1))
  }

  /* Fails with String bNodes test("insert 4 triples with bnodes") {
    val t1 = RDFTriple(BNodeId("b" + 0), IRI("b"), BNodeId("b" + 1))
    val t2 = RDFTriple(BNodeId("b" + 1), IRI("b"), BNodeId("b" + 2))
    val t3 = RDFTriple(BNodeId("b" + 1), IRI("b"), BNodeId("b" + 3))
    val t4 = RDFTriple(BNodeId("b" + 1), IRI("b"), IRI("c"))
    val g = RDFGraph.empty.addTriples(Set(t1, t2, t3, t4))
    assert(g.triples(BNodeId("b" + 0)) === Set(t1, t2, t3, t4))
  } */

  test("insert 4 triples with bnodes identical") {
    val t1 = RDFTriple(BNodeId("b" + 0), IRI("b"), BNodeId("b" + 0))
    val t2 = RDFTriple(BNodeId("b" + 1), IRI("b"), BNodeId("b" + 1))
    val t3 = RDFTriple(BNodeId("b" + 1), IRI("b"), BNodeId("b" + 1))
    val t4 = RDFTriple(BNodeId("b" + 1), IRI("b"), IRI("c"))
    val g = RDFGraph.empty.addTriples(Set(t1, t2, t3, t4))
    assert(g.triples(BNodeId("b" + 0)) === Set(t1, t2, t3, t4))
  }

  test("Add 1 triple without bnodes in two phases") {
    val tabc = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
    val tabd = RDFTriple(IRI("a"), IRI("b"), IRI("d"))
    val g = RDFGraph.empty.addTriples(Set(tabc))
    val g1 = g.addTriples(Set(tabd))
    assert(g1.triples(BNodeId("b" + 0)) === Set(tabc, tabd))
  }

  test("Add 2 triples without bnodes in two phases") {
    val tabc = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
    val tcba = RDFTriple(IRI("c"), IRI("b"), IRI("a"))
    val tabd = RDFTriple(IRI("a"), IRI("b"), IRI("d"))
    val tdba = RDFTriple(IRI("d"), IRI("b"), IRI("a"))
    val g = RDFGraph.empty.addTriples(Set(tabc, tcba))
    val g1 = g.addTriples(Set(tabd, tdba))
    assert(g1.triples(BNodeId("b" + 0)) === Set(tabc, tcba, tabd, tdba))
  }

  test("Add 2 triples with bnodes in two phases") {
    val tab0 = RDFTriple(IRI("a"), IRI("b"), BNodeId("b" + 0))
    val tac0 = RDFTriple(IRI("a"), IRI("c"), BNodeId("b" + 0))
    val t0ca = RDFTriple(BNodeId("b" + 0), IRI("c"), IRI("a"))
    val tac1 = RDFTriple(IRI("a"), IRI("c"), BNodeId("b" + 1))
    val t1ca = RDFTriple(BNodeId("b" + 1), IRI("c"), IRI("a"))
    val g = RDFGraph.empty.addTriples(Set(tab0))
    val g1 = g.addTriples(Set(tac0, t0ca))
    assert(g1.triples(BNodeId("b" + 0)) === Set(tab0, tac1, t1ca))
  }

  test("merge 2 graphs without BNodes") {
    val tabc = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
    val tcba = RDFTriple(IRI("c"), IRI("b"), IRI("a"))
    val tabd = RDFTriple(IRI("a"), IRI("b"), IRI("d"))
    val tdba = RDFTriple(IRI("d"), IRI("b"), IRI("a"))
    val g1 = RDFGraph.empty.addTriples(Set(tabc, tcba))
    val g2 = RDFGraph.empty.addTriples(Set(tabd, tdba))
    assert(g1.merge(g2).triples(BNodeId("b" + 0)) === Set(tabc, tcba, tabd, tdba))
  }

  test("merge graphs with BNodes") {
    val tab0 = RDFTriple(IRI("a"), IRI("b"), BNodeId("b" + 0))
    val t0ba = RDFTriple(BNodeId("b" + 0), IRI("b"), IRI("a"))
    val t1ba = RDFTriple(BNodeId("b" + 1), IRI("b"), IRI("a"))
    val g1 = RDFGraph.empty.addTriples(Set(tab0))
    val g2 = RDFGraph.empty.addTriples(Set(t0ba))
    val merge = g1.merge(g2)

    assert(g1.merge(g2).triples(BNodeId("b" + 0)) ===
      Set(tab0, t1ba))
  }

  test("merge 2 graphs. One with a triple and one with two triples") {
    val tab0 = RDFTriple(IRI("a"), IRI("b"), BNodeId("b" + 0))
    val tac0 = RDFTriple(IRI("a"), IRI("c"), BNodeId("b" + 0))
    val t0ca = RDFTriple(BNodeId("b" + 0), IRI("c"), IRI("a"))
    val tac1 = RDFTriple(IRI("a"), IRI("c"), BNodeId("b" + 1))
    val t1ca = RDFTriple(BNodeId("b" + 1), IRI("c"), IRI("a"))
    val g1 = RDFGraph.empty.addTriples(Set(tab0))
    val g2 = RDFGraph.empty.addTriples(Set(tac0, t0ca))
    val merge = g1.merge(g2)

    assert(g1.merge(g2).triples(BNodeId("b" + 0)) === Set(tab0, tac1, t1ca))
  }

  test("merge 2 graphs with BNodes") {
    val tab0 = RDFTriple(IRI("a"), IRI("b"), BNodeId("b" + 0))
    val t0ba = RDFTriple(BNodeId("b" + 0), IRI("b"), IRI("a"))
    val tab1 = RDFTriple(IRI("a"), IRI("b"), BNodeId("b" + 1))
    val tac0 = RDFTriple(IRI("a"), IRI("c"), BNodeId("b" + 0))
    val t0ca = RDFTriple(BNodeId("b" + 0), IRI("c"), IRI("a"))
    val tac2 = RDFTriple(IRI("a"), IRI("c"), BNodeId("b" + 2))
    val t2ca = RDFTriple(BNodeId("b" + 2), IRI("c"), IRI("a"))
    val g1 = RDFGraph.empty.addTriples(Set(tab0, t0ba, tab1))
    val g2 = RDFGraph.empty.addTriples(Set(tac0, t0ca))
    val merge = g1.merge(g2)

    assert(g1.merge(g2).triples(BNodeId("b" + 0)) === Set(tab0, t0ba, tab1, tac2, t2ca))
  }

  test("foldRDFGraph_1_triple") {
    val tabc = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
    val g1 = RDFGraph.empty.addTriples(Set(tabc))
    assert(g1.foldRDFGraph(0, (n: Int, ctx: TContext[RDFNode]) => 1 + n) === 3)
  }

  test("foldRDFGraph_cycle") {
    val tabc = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
    val tcba = RDFTriple(IRI("c"), IRI("b"), IRI("a"))
    val g1 = RDFGraph.empty.addTriples(Set(tabc, tcba))
    assert(g1.foldRDFGraph(0, (n: Int, ctx: TContext[RDFNode]) => 1 + n) === 3)
  }

  test("foldRDF_4triples") {
    val tabc = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
    val tbcb = RDFTriple(IRI("b"), IRI("c"), IRI("b"))
    val tcba = RDFTriple(IRI("c"), IRI("b"), IRI("a"))
    val tbcd = RDFTriple(IRI("b"), IRI("c"), IRI("d"))
    val g1 = RDFGraph.empty.addTriples(Set(tabc, tcba, tbcb, tbcd))
    assert(g1.foldRDFGraph(0, (n: Int, ctx: TContext[RDFNode]) => 1 + n) === 4)
  }

  test("foldRDF_cycle") {
    val tabc = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
    val tcba = RDFTriple(IRI("c"), IRI("b"), IRI("a"))
    val g1 = RDFGraph.empty.addTriples(Set(tabc, tcba))
    def empty: Set[RDFNode] = Set()
    def add(ctx: TContext[RDFNode], set: Set[RDFNode]): Set[RDFNode] = set + ctx.node
    assert(g1.foldRDFGraph(Set(): Set[RDFNode],
      (set: Set[RDFNode], ctx: TContext[RDFNode]) => set + ctx.node)
      === Set(IRI("a"), IRI("b"), IRI("c")))
  }

  test("foldRDF_4triples_toSet") {

    def empty: Set[RDFNode] = Set()
    def add(set: Set[RDFNode], ctx: TContext[RDFNode]): Set[RDFNode] = set + ctx.node

    val tabc = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
    val tbcb = RDFTriple(IRI("b"), IRI("c"), IRI("b"))
    val tcba = RDFTriple(IRI("c"), IRI("b"), IRI("a"))
    val tbcd = RDFTriple(IRI("b"), IRI("c"), IRI("d"))
    val g1 = RDFGraph.empty.addTriples(Set(tabc, tcba, tbcb, tbcd))
    assert(g1.foldRDFGraph(empty, add) === Set(IRI("a"), IRI("b"), IRI("c"), IRI("d")))
  }

}
