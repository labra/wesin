package es.weso.qgraph

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import es.weso.tgraph.TGraph._
import es.weso.tgraph.TGraphImpl._
import scalax.collection.immutable.Graph
import scalax.collection.GraphEdge._
import org.scalatest.Matchers

@RunWith(classOf[JUnitRunner])
class QGraphSuite extends FunSuite with Matchers {

  test("empty qgraph") {
    val g : QGraph[Int,Int] = QGraph.empty
    g.isEmpty should be (true)
  }
  
  test("qgraph with x { a p b , b p a }") {
    val g0 : QGraph[Char,Char] = QGraph.empty
    val g1 = g0.addTriple(('a','p','b'),Some('x')).addTriple(('b','p','a'),Some('x'))
    g1.default.isEmpty should be (true) 
    g1.getGraph('x').get.triples.size should be(2)
  }
  
  test("qgraph with x { a p b , b p a }, y { a p a } ") {
    val g : QGraph[Char,Char] = QGraph.empty
    val g1 = g.addTriple(('a','p','b'),Some('x')).
    		   addTriple(('b','p','a'),Some('x')).
    		   addTriple(('a','p','a'),Some('y'))
    g1.default.isEmpty should be (true) 
    g1.getGraph('x').get.triples.size should be(2)
    g1.getGraph('y').get.triples.size should be(1)
  }
  
}
