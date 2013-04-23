package org.weso.graph

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LBase.LEdgeImplicits
import scala.collection.immutable.Set



case class TGraphImpl[A](graph: Graph[A,DiHyperEdge]) extends TGraph[A] {

  // TODO: Consider defining a custom edge instead of DiHyperEdge

  def empty = TGraphImpl(graph.empty)

  def nodes = graph.nodes.map(_.value).toSet

  def decomp (node : A) = {
    if (graph.isEmpty) None 
    else for {
      p <- pred(node)
      s <- succ(node)
      r <- rels(node)
     } yield (Context(node,p,s,r),this.remove(node))  
  }

  def extend(ctx : Context[A]) = {
    TGraphImpl( 
         ((((graph + ctx.node) 
         /: ctx.succ) { (g,p) => g + (ctx.node ~> p._1 ~> p._2) }
         /: ctx.pred) { (g,p) => g + (p._1 ~> p._2 ~> ctx.node) }
         /: ctx.rels) { (g,p) => g + (p._1 ~> ctx.node ~> p._2) }
      )
  }    
  
  override def isEmpty = graph.isEmpty

  override def addTriple (triple: (A,A,A)) : TGraphImpl[A] = {
    TGraphImpl(graph + (triple._1 ~> triple._2 ~> triple._3))
  }

  override def addNode (node : A) : TGraphImpl[A] = {
    TGraphImpl(graph + node)
  }

  override def deleteNode (node : A) : TGraphImpl[A] = {
    TGraphImpl(graph - node)
  }

  
  
 /**
   * List of successors: outgoing edges (rel,destiny)
   * @param node resource 
   */
  def succ(node:A) : Option[Set[(A,A)]] = {
    graph.find(node).map { 
    	_.edges.
    	filter(_._1 == node).
    	map((e) => (e._2.value,e.last.value)).
    	toSet
    }
  }
  
  /**
   * List of predecessors: incoming edges (origin,rel)
   * @param node resource 
   */
  def pred(node:A) : Option[Set[(A,A)]] = {
    graph.find(node).map{ 
       _.edges.
       filter( _.last == node) . 
       map((e) => (e._1.value,e._2.value)).
       toSet
    }
  }

  /**
   * List of related nodes: (origin,destiny)
   * @param node resource 
   */
  def rels(node:A) : Option[Set[(A,A)]] = {
    graph.find(node).map {
      _.edges.
      filter(_._2 == node).
      map((e) => (e._1.value,e.last.value)).
      toSet
    }
  }

  def remove(node : A) : TGraphImpl[A] = {
    TGraphImpl(graph - node)
  }


}


