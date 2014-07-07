package es.weso.tgraph

import scala.collection.immutable.Set
import scalax.collection.immutable.Graph
import scala.annotation.tailrec

/*
 * Generic triple graphs
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
   * @param node to remove
   */
  def decomp (node : A) : Option[(TContext[A],TGraph[A])]
  
  def decompAny : Option[(TContext[A],TGraph[A])] = {
    if (this.isEmpty) None
    else decomp(nodes.head)
  }
  
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
   	 { (r,ctx) => ctx.triples ++ r }  
  }
  
  def foldTGraph[B](accum:B)(f:(B,TContext[A]) => B): B =
    foldTGraphByDecomp(accum)(f)
  
  /*
   * Implementation assuming an efficient `decomp`.  
   */
  @tailrec private
  def foldTGraphByDecomp[B](accum:B)(f:(B,TContext[A]) => B): B = {
    decompAny match {
     case None => accum
     case Some((ctx,rest)) => {
       val current = f(accum,ctx)
       rest.foldTGraphByDecomp(current)(f)
     }
    }
  }
  
  def foldTGraphOrd[B](accum:B)(f:(B,TContext[A]) => B)
                      (implicit ord: Ordering[A]) : B =
    foldTGraphOrdByDecomp(accum)(f)

  /*
   * Implementation assuming an efficient `decomp`.  
   */
  @tailrec private
  def foldTGraphOrdByDecomp[B](accum:B)(f:(B,TContext[A]) => B)
                              (implicit ord: Ordering[A]) : B = {
   if (this.isEmpty) accum 
   else {
    decomp(nodes.min(ord)) match {
     case None => accum
     case Some((ctx,rest)) => {
       val current = f(accum,ctx)
       rest.foldTGraphOrdByDecomp(current)(f)
     }
    }
   }
  }
  
  def map[B : Manifest](f : A => B) : TGraph[B]

}

object TGraph {
 
  // TODO: Remove these declarations from here...
  //       It makes this code dependent on the TGraphImpl
  def empty [A : Manifest]: TGraph[A] =
    TGraphImpl(Graph[A,Triple]())
    
  def fromTriple[A: Manifest] (triple: (A,A,A)): TGraph[A] = {
    TGraph.empty.addTriple(triple)
  }
    
}