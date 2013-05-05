package org.weso.graph

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.weso.graph.TGraph._
import org.weso.graph.TGraphImpl._

import scalax.collection.immutable.Graph
import scalax.collection.GraphEdge._

@RunWith(classOf[JUnitRunner])
class GraphSuite extends FunSuite {

  test("empty graph") {
    val g = new TGraphImpl(Graph[Int,DiHyperEdge]())
    assert(g.isEmpty,true)
  }
  
  test("simple triple contains origin") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple(('a','b','c'))
    assert(g1.nodes.contains('a'))
  }

  test("simple triple contains edge") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple(('a','b','c'))
    assert(g1.nodes.contains('b'))
  }

  test("simple triple contains destiny") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple(('a','b','c'))
    assert(g1.nodes.contains('c'))
  }


  test("decomp with two triples, a") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    val g2 = g1.addTriple('a','b','d')
    assert(g2.decomp('a').get._1.pred === Set())
    assert(g2.decomp('a').get._1.succ === Set(('b','c'),('b','d')))
    assert(g2.decomp('a').get._1.rels === Set())
  }

  test("decomp with two triples, b") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    val g2 = g1.addTriple('a','b','d')
    assert(g2.decomp('b').get._1.pred === Set())
    assert(g2.decomp('b').get._1.succ === Set())
    assert(g2.decomp('b').get._1.rels === Set(('a','c'),('a','d')))
  }

  test("decomp with two triples, c") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    val g2 = g1.addTriple('a','b','d')
    assert(g2.decomp('c').get._1.pred === Set(('a','b')))
    assert(g2.decomp('c').get._1.succ === Set())
    assert(g2.decomp('c').get._1.rels === Set())
  }

  test("triples, a b c") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    assert(g1.triples == Set(('a','b','c')))
  }

  test("triples, abc, abd") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    val g2 = g1.addTriple('a','b','d')
    assert(g2.triples == Set(('a','b','c'),('a','b','d')))
  }

  test("foldGraph min") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('b','c','a')
    val g2 = g1.addTriple('a','b','d')
    val g = g2
    val ls = g.foldTGraphOrd(List[Char]())((ctx,r) => ctx.node :: r)
    assert(ls == List('a','b','c','d'))
  }

  test("foldGraph min loop") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('a','b','c')
    val g2 = g1.addTriple('c','b','d')
    val g3 = g2.addTriple('d','b','a')
    val g = g3
    val ls = g.foldTGraphOrd(List[Char]())((ctx,r) => ctx.node :: r)
    assert(ls === List('a','b','c','d'))
  }

  test("foldGraph max") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val g1 = g0.addTriple('b','c','a')
    val g2 = g1.addTriple('a','b','d')
    val g = g2
    val ord = new Ordering[Char] { def compare(x :Char,y :Char):Int = y - x } 
    val ls = g.foldTGraphOrd(List[Char]())((ctx,r) => ctx.node :: r)(ord)
    assert(ls === List('d','c','b','a'))
  }

  test("map empty") {
    val g0 = new TGraphImpl(Graph[Int,DiHyperEdge]())
    val g = g0.map((x:Int) => x + 1)
    assert(g.nodes === Set())
  }

  test("mapGraph (+1)") {
    val g0 = new TGraphImpl(Graph[Int,DiHyperEdge]())
    val g1 = g0.addTriple(1,2,3)
    val g2 = g1.addTriple(3,2,1)

    val gN = g2.map((x:Int) => x + 1)
    assert(gN.nodes === Set(2,3,4))
  }

  test("map toString") {
    val g0 = new TGraphImpl(Graph[Int,DiHyperEdge]())
    val g1 = g0.addTriple(1,2,3)
    val g2 = g1.addTriple(3,2,1)

    val gN = g2.map((x:Int) => x.toString)
    assert(gN.nodes === Set("1","2","3"))
  }

  test("extend empty with a list of succ") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val e = Set[(Char,Char)]()
    val ctx = Context('a',e,Set(('b','c'),('b','d')),e)
    val g1 = g0.extend(ctx)
    assert(g1.triples === Set(('a','b','c'),('a','b','d')))
  }

  test("extend empty with a list of pred") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val e = Set[(Char,Char)]()
    val ctx = Context('a',Set(('b','c'),('b','d')),e,e)
    val g1 = g0.extend(ctx)
    assert(g1.triples === Set(('b','c','a'),('b','d','a')))
  }

  test("extend empty with a list of rels") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val e = Set[(Char,Char)]()
    val ctx = Context('a',e,e,Set(('b','c'),('b','d')))
    val g1 = g0.extend(ctx)
    assert(g1.triples === Set(('b','a','c'),('b','a','d')))
  }

  test("extend empty with succ, rels and pred") {
    val g0 = new TGraphImpl(Graph[Char,DiHyperEdge]())
    val e = Set[(Char,Char)]()
    val ctx = Context('x',
    		Set(('a','b'),('c','d')),
    		Set(('e','f'),('g','h')),
    		Set(('i','j'),('k','l'))
        )
    val g1 = g0.extend(ctx)
    assert(g1.triples === 
      	Set(('a','b','x'),('c','d','x'),
      	    ('x','e','f'),('x','g','h'),
      	    ('i','x','j'),('k','x','l')))
  }
}
