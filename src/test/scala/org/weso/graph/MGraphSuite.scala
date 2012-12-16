package org.weso.graph

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.weso.graph.MGraph._
import org.weso.graph.MGraphImpl._

import scalax.collection.Graph
import scalax.collection.GraphEdge._

@RunWith(classOf[JUnitRunner])
class GraphSuite extends FunSuite {

  test("empty graph") {
    val g = new MGraphImpl(Graph[Int,DiHyperEdge]())
    assert(g.isEmpty,true)
  }
  
  test("simple triple contains origin") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple(('a','b','c'))
    assert(g1.nodes.contains('a'))
  }

  test("simple triple contains edge") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple(('a','b','c'))
    assert(g1.nodes.contains('b'))
  }

  test("simple triple contains destiny") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple(('a','b','c'))
    assert(g1.nodes.contains('c'))
  }


  test("decomp with two triples, a") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    val g2 = g1.addTriple('a','b','d')
    assert(g2.decomp('a').get._1.pred === Set())
    assert(g2.decomp('a').get._1.succ === Set(('b','c'),('b','d')))
    assert(g2.decomp('a').get._1.rels === Set())
  }

  test("decomp with two triples, b") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    val g2 = g1.addTriple('a','b','d')
    assert(g2.decomp('b').get._1.pred === Set())
    assert(g2.decomp('b').get._1.succ === Set())
    assert(g2.decomp('b').get._1.rels === Set(('a','c'),('a','d')))
  }

  test("decomp with two triples, c") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    val g2 = g1.addTriple('a','b','d')
    assert(g2.decomp('c').get._1.pred === Set(('a','b')))
    assert(g2.decomp('c').get._1.succ === Set())
    assert(g2.decomp('c').get._1.rels === Set())
  }

  test("triples, a b c") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    assert(g1.triples == Set(('a','b','c')))
  }

  test("triples, abc, abd") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    val g2 = g1.addTriple('a','b','d')
    assert(g2.triples == Set(('a','b','c'),('a','b','d')))
  }

  test("foldGraph min") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('b','c','a')
    val g2 = g1.addTriple('a','b','d')
    val g = g2
    val ls = g.foldMGraphOrd(List[Char]())((ctx,r) => ctx.node :: r)
    assert(ls == List('a','b','c','d'))
  }

  test("foldGraph min loop") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    val g2 = g1.addTriple('c','b','d')
    val g3 = g2.addTriple('d','b','a')
    val g = g3
    val ls = g.foldMGraphOrd(List[Char]())((ctx,r) => ctx.node :: r)
    assert(ls === List('a','b','c','d'))
  }

  test("foldGraph max") {
    val g0 = new MGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('b','c','a')
    val g2 = g1.addTriple('a','b','d')
    val g = g2
    val ord = new Ordering[Char] { def compare(x :Char,y :Char):Int = y - x } 
    val ls = g.foldMGraphOrd(List[Char]())((ctx,r) => ctx.node :: r)(ord)
    assert(ls === List('d','c','b','a'))
  }
}
