package es.weso.tgraph

import scalax.collection.immutable.Graph
import scala.collection.immutable.Set

case class TGraphImpl[A](graph: Graph[A,Triple]) extends TGraph[A] {

  def empty = TGraphImpl(graph.empty)

  def nodes = graph.nodes.map(_.value).toSet[A]

  def decomp (node : A) = {
    if (graph.isEmpty) None 
    else for {
      p <- pred(node)
      s <- succ(node)
      r <- rels(node)
     } yield (TContext(node,p,s,r),this.remove(node))  
  }

  def extend(ctx : TContext[A]) = {
    TGraphImpl( 
         ((((graph + ctx.node) 
         /: ctx.succ) { (g,p) => g + Triple(ctx.node,p._1,p._2) }
         /: ctx.pred) { (g,p) => g + Triple(p._1,p._2,ctx.node) }
         /: ctx.rels) { (g,p) => g + Triple(p._1,ctx.node,p._2) }
      )
  }    
  
  override def isEmpty = graph.isEmpty

  override def addTriple (triple: (A,A,A)) : TGraphImpl[A] = {
    TGraphImpl(graph + Triple(triple._1,triple._2,triple._3))
  }

  override def foldTGraph[B](accum:B)(f:(B,TContext[A]) => B): B =
    fold(graph.nodes)(accum)(f)
    
  override def foldTGraphOrd[B](accum:B)(f:(B,TContext[A]) => B)
                               (implicit ord: Ordering[A]) : B = {
    val nodeTOrdering = Ordering.by[graph.NodeT,A](_.value)
    val sorted = graph.nodes.toList.sorted(nodeTOrdering)
    fold(sorted)(accum)(f)
  }
  
  private def fold[B](nodes: Traversable[graph.NodeT])
                     (accum:B)(f:(B,TContext[A]) => B): B = {
    (accum /: nodes)((accum:B, node:graph.NodeT) =>
      f(accum, TContext(node.value, pred(node), succ(node), rels(node))))
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
    graph.find(node).map(succ)
  }
  
  def succ(node: graph.NodeT) : Set[(A,A)] = {
    node.edges.
      withFilter(_._1 == node).
      map((e) => (e._2.value,e.last.value)).
      toSet[(A,A)]
  }
  
  /**
   * List of predecessors: incoming edges (origin,rel)
   * @param node resource 
   */
  def pred(node:A) : Option[Set[(A,A)]] = {
    graph.find(node).map(pred)
  }

  def pred(node: graph.NodeT) : Set[(A,A)] = {
    node.edges.
       filter( _.last == node) . 
       map((e) => (e._1.value,e._2.value)).
       toSet[(A,A)]
  }

  /**
   * List of related nodes: (origin,destiny)
   * @param node resource 
   */
  def rels(node:A) : Option[Set[(A,A)]] = {
    graph.find(node).map(rels)
  }

  def rels(node: graph.NodeT) : Set[(A,A)] = {
    node.edges.
      filter(_._2 == node).
      map((e) => (e._1.value,e.last.value)).
      toSet[(A,A)]
  }

  def remove(node : A) : TGraphImpl[A] = {
    TGraphImpl(graph - node)
  }

  def map[B : Manifest](f : A => B) : TGraph[B] = {
    this.decompAny match {
      case None => TGraphImpl(Graph[B,Triple]())
      case Some((ctx,g)) => g.map(f).extend(ctx.map(f))
    } 
  }
}


