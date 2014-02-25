package es.weso.qgraph

import es.weso.tgraph._
import scala.collection.immutable.HashMap

/*
 * Generic quad graphs
 * 
 * Graphs with values of type A that are indexed by B 
 */
trait QGraph[A,B] {
  
  /**
   * Returns the default graph
   * 
   */
  def default() : TGraph[A] 
  
  /** Empty quad-graph 
   **/
  def empty() : QGraph[A,B]
  
  def isEmpty : Boolean
  
  /** Adds a graph to the quad-graph structure. 
   *  If the index is None, it changes the default graph
   **/ 
  def addGraph(graph: TGraph[A], index: Option[B]): QGraph[A,B]
  
  /**
   * Remove the graph indexed by `index` 
   * 
   */
  
  def rmGraph(index: B): QGraph[A,B]
  
  /**
   * Adds a triple to a quad-graph
   * If the index is None it adds the triple to the default graph
   */
   def addTriple(triple: (A,A,A), index: Option[B]): QGraph[A,B] 
  
  def getGraph(index:B): Option[TGraph[A]] 

}

object QGraph {

  def empty[A, B : Ordering] : QGraph[A,B] = {

    // TODO: Can I simplify this declaration?
    //Anton: what is the reason for having HashMap?
	QGraphImpl(TGraph.empty.asInstanceOf[TGraph[A]], 
			   HashMap[B,TGraph[A]]())
  }
  
}