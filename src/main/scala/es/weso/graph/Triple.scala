package es.weso.graph

import scalax.collection.GraphPredef.EdgeIn
import scalax.collection.GraphEdge.DiHyperEdge
import scalax.collection.GraphEdge.EdgeCopy

class Triple[A](nodes: (A,A,A)) 
    extends DiHyperEdge[A](nodes)
    with    EdgeCopy[Triple]
    with    EdgeIn[A,Triple] {

  override def copy[B](newNodes: Product) =
    new Triple[B](newNodes.asInstanceOf[(B,B,B)])
 
  def subj : A = { nodes._1 }
  def pred : A = { nodes._2 }
  def obj  : A = { nodes._3 }
}

object Triple {
	def apply[A](subj: A, pred: A, obj : A) =
			new Triple[A]((subj,pred,obj))
	def unapply[A](e: Triple[A]) = Some(e)
}