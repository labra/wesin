package org.weso.rdfTriple

import org.weso.rdfNode._
import scala.collection.Set

case class RDFTriple(subj : RDFNode, pred : IRI, obj : RDFNode) {
  def extractBNode (node: RDFNode) : Set[BNodeId] = {
    node match {
      case b@BNodeId(id) => Set(b)
      case _ => Set()
    }
  }
    
  def bNodes : Set[BNodeId] = {
    val set = Set()
    set ++ extractBNode(subj) ++ extractBNode(obj)
  }
  
  override def toString : String = {
    subj + " " + pred + " " + obj + " ."
  }
}

object RDFTriple {
  
  /**
   *  Constructor of RDFTriples from triples
   */ 
  def apply(triple:(RDFNode,IRI,RDFNode)) 
  	= new RDFTriple(triple._1,triple._2,triple._3)

/**
 * collects BNodes in a set of triples
 */
def collectBNodes (triples : Set[RDFTriple]) : Set[BNodeId] = {
    triples.foldLeft (Set[BNodeId]()) ((set,triple) => 
    										set ++ triple.bNodes)
}	

  /**
   * Shows a set of triples
   * TODO: Consider removing this method
   */
  def showTriples( triples : Set[RDFTriple]) : String = {
  var str = new StringBuilder
  for { t <- triples} { str ++= (t + "\n") 
  }
  str.toString
}

}



