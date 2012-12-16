package org.weso.graph

import scala.collection.Set



case class Context[A](
		node : A, 
		pred: Set[(A,A)], 
		succ: Set[(A,A)], 
		rels: Set[(A,A)]
) {
  def triples : Set[(A,A,A)] = {
    succ.foldLeft(Set[(A,A,A)]()) { (r,p) => Set((node,p._1,p._2)) ++ r } ++ 
    pred.foldLeft(Set[(A,A,A)]()) { (r,p) => Set((p._1,p._2,node)) ++ r } ++ 
    rels.foldLeft(Set[(A,A,A)]()) { (r,p) => Set((p._1,node,p._2)) ++ r }
   }
  
}

/*
 * Generic graphs 
 */
trait MGraph[A] {
  
  /*
   * Returns empty graph
   */
  def empty : MGraph[A]
  
  /** Add a list of triples to a graph 
   * @param triples to add
   */
  def mkGraph (triples : Set[(A,A,A)]) : MGraph[A] = {
    triples.foldLeft(this) ((g,t) => g.addTriple(t))
  }
  
  /**
   * List of nodes 
   */
  def nodes : Set[A]
  
  /*
   * Decompose a graph
   * 
   * @param node to insert
   */
  def decomp (node : A) : Option[(Context[A],MGraph[A])]
  
  /*
   * Extend a graph with a new context. 
   * We define this method using 'addNode' and 'addTriple' but it could be the other way
   * 
   * @param context to add
   */
  def extend (ctx : Context[A]) : MGraph[A] = {
    val n = ctx.node
    val g1 = this.addNode(n)
    val g2 = ctx.triples.foldLeft(g1)((g,t) => g.addTriple(t))
    g2
  }

  /*
   * Add a node to a graph
   */
  def addNode (node : A) : MGraph[A]
  
  /*
   * Add a triple to a graph
   */
  def addTriple (triple : (A,A,A)) : MGraph[A]
  
  /*
   * True if graph is empty
   */
  def isEmpty : Boolean = {
    this.nodes == Set[A]()
  }

  def deleteNode(node : A) : MGraph[A] = {
    this.decomp(node) match {
      case None => this
      case Some(dec) => dec._2
    }   
  }
  
  def triples : Set[(A,A,A)] = {
   foldMGraph(Set[(A,A,A)]())
   	 { (ctx,r) => ctx.triples ++ r }  
  }
  
  def foldMGraph[B](e:B)(f:(Context[A],B) => B): B = {
   if (this.isEmpty) e 
   else {
    decomp(nodes.head) match {
     case None => e
     case Some(dec) => f(dec._1,dec._2.foldMGraph(e)(f))
     }
    }
  }
  
  def foldMGraphOrd[B](e:B)(f:(Context[A],B) => B)(implicit ord: Ordering[A]) : B = {
   if (this.isEmpty) e 
   else {
    decomp(nodes.min) match {
     case None => e
     case Some(dec) => f(dec._1,dec._2.foldMGraphOrd(e)(f))
     }
    }
  }
}

object MGraph {
  
  

}