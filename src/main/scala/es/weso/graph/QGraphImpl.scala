package es.weso.graph

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LBase.LEdgeImplicits
import scala.collection.immutable.Set
import scala.collection.immutable.HashMap



case class QGraphImpl[A,B](
    default: TGraph[A],
    map: 	 HashMap[B,TGraph[A]]) extends QGraph[A,B] {

  def empty = QGraphImpl(default.empty, HashMap())
  
  def addGraph(graph: TGraph[A], index: Option[B]): QGraph[A,B] = ???
  
  def rmGraph(index: B): QGraph[A,B] = ???
  
  def addTriple(triple: (A,A,A), index: Option[B]): QGraph[A,B] = ???


}


