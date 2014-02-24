package es.weso.graph

import scala.collection.immutable.Set

/*
 * Generic graphs 
 */
trait TGraph[A] {
  
  /*
   * Returns empty graph
   */
  def empty : TGraph[A]
  
  /** Add a list of triples to a graph 
   * @param triples to add
   */
  def mkGraph (triples : Set[(A,A,A)]) : TGraph[A] = {
    triples.foldLeft(this) ((g,t) => g.addTriple(t))
  }
  
  /**
   * List of nodes 
   */
  def nodes : Set[A]
  
  /*
   * Decompose a graph
   * @param node to insert
   */
  def decomp (node : A) : Option[(TContext[A],TGraph[A])]
  
  /*
   * Extend a graph with a new context. 
   * @param context to add
   */
  def extend (ctx : TContext[A]) : TGraph[A]

  /*
   * Add a node to a graph
   */
  def addNode (node : A) : TGraph[A]
  
  /*
   * Add a triple to a graph
   */
  def addTriple (triple : (A,A,A)) : TGraph[A]
  
  /*
   * True if graph is empty
   */
  def isEmpty : Boolean = {
    this.nodes == Set[A]()
  }

  def deleteNode(node : A) : TGraph[A] = {
    this.decomp(node) match {
      case None => this
      case Some(dec) => dec._2
    }   
  }
  
  def triples : Set[(A,A,A)] = {
   foldTGraph(Set[(A,A,A)]())
   	 { (ctx,r) => ctx.triples ++ r }  
  }
  
  def foldTGraph[B](e:B)(f:(TContext[A],B) => B): B = {
   if (this.isEmpty) e 
   else {
    decomp(nodes.head) match {
     case None => e
     case Some(dec) => f(dec._1,dec._2.foldTGraph(e)(f))
     }
    }
  }
  
  def foldTGraphOrd[B](e:B)(f:(TContext[A],B) => B)(implicit ord: Ordering[A]) : B = {
   if (this.isEmpty) e 
   else {
    decomp(nodes.min) match {
     case None => e
     case Some(dec) => f(dec._1,dec._2.foldTGraphOrd(e)(f))
     }
    }
  }
  
  def map[B : Manifest](f : A => B) : TGraph[B]

}

object TGraph {
}