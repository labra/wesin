package org.weso.graph

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LBase.LEdgeImplicits
import scala.collection.Set

case class MGraphImpl[A](graph: Graph[A,DiHyperEdge]) extends MGraph[A] {

  def empty : MGraphImpl[A] = MGraphImpl(graph.empty)
  
  override def isEmpty : Boolean =
    graph.isEmpty

  override def addTriple (triple: (A,A,A)) : MGraphImpl[A] = {
    MGraphImpl(graph + (triple._1 ~> triple._2 ~> triple._3))
  }

  override def addNode (node : A) : MGraphImpl[A] = {
    MGraphImpl(graph + node)
  }

  override def deleteNode (node : A) : MGraphImpl[A] = {
    MGraphImpl(graph - node)
  }

  override def nodes = {
    graph.nodes.map(_.value)
  }
  
  override def decomp (node : A) : Option[(Context[A], MGraphImpl[A])] = {
    if (graph.isEmpty) None 
    else for {
      p <- pred(node)
      s <- succ(node)
      r <- rels(node)
     } yield (Context(node,p,s,r),this.remove(node))  
  }
  
  
 /**
   * List of successors: outgoing edges (rel,destiny)
   * @param node resource 
   */
  def succ(node:A) : Option[Set[(A,A)]] = {
    for { 
      n <- graph.find(node)
    } yield 
    	n.edges.
    	filter((e) => e._1 == node).
    	map((e) => (e._2.value,e.last.value))
  }
  
  /**
   * List of predecessors: incoming edges (origin,rel)
   * @param node resource 
   */
  def pred(node:A) : Option[Set[(A,A)]] = {
    for { 
      n <- graph.find(node)
    } yield 
    	n.edges.
    	filter((e) => e.last == node).
    	map((e) => (e._1.value,e._2.value))
  }

  /**
   * List of related nodes: (origin,destiny)
   * @param node resource 
   */
  def rels(node:A) : Option[Set[(A,A)]] = {
    for { 
      n <- graph.find(node)
    } yield 
    	n.edges.
    	filter((e) => e._2 == node).
    	map((e) => (e._1.value,e.last.value))
  }

  def remove(node : A) : MGraphImpl[A] = {
    MGraphImpl(graph - node)
  }


}


