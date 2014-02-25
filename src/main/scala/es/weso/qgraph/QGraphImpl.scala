package es.weso.qgraph

import scala.collection.immutable.HashMap
import es.weso.tgraph._


case class QGraphImpl[A,B](
    default: TGraph[A],
    map: 	 HashMap[B,TGraph[A]]) extends QGraph[A,B] {

  def empty : QGraphImpl[A,B] = QGraphImpl(default.empty, HashMap())
  
  def isEmpty : Boolean = default.isEmpty && map.isEmpty
  
  def addGraph(graph: TGraph[A], index: Option[B]): QGraph[A,B] =
    index match {
    	case None 		=> QGraphImpl(graph,map)
    	case Some(idx) 	=> QGraphImpl(default, map + ((idx,graph))) 
  }
  
  def rmGraph(index: B): QGraph[A,B] = ???
  
  def addTriple(triple: (A,A,A), index: Option[B]): QGraph[A,B] = {
    index match {
      case None => QGraphImpl(default.addTriple(triple),map)
      
      case Some(idx) => {
        getGraph(idx) match {
          case None    => QGraphImpl(default, map + ((idx, fromTriple(triple)))) 
          case Some(g) => QGraphImpl(default, map + ((idx, g.addTriple(triple))))
        }
      }
    }
  }
  
  def fromTriple[A] (triple: (A,A,A)): TGraph[A] = {
    TGraph.empty.asInstanceOf[TGraph[A]].addTriple(triple)
  }

  def getGraph(index:B) : Option[TGraph[A]] = 
    map.get(index)
}

object QGraphImpl {
}


