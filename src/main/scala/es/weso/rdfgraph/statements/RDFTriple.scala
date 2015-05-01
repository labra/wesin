package es.weso.rdfgraph.statements

import scala.collection.Set
import es.weso.rdfgraph.nodes._

case class RDFTriple(subj: RDFNode, pred: IRI, obj: RDFNode) {

  def hasSubject(node: RDFNode): Boolean = subj == node
  def hasObject(node: RDFNode): Boolean = obj == node
  def hasPredicate(p: IRI): Boolean = pred == p

  def extractBNode(node: RDFNode): Set[BNodeId] = {
    node match {
      case b @ BNodeId(id) => Set(b)
      case _ => Set()
    }
  }

  def extractIRI(node: RDFNode): Set[IRI] = {
    node match {
      case iri: IRI => Set(iri)
      case _ => Set()
    }
  }

  def bNodes: Set[BNodeId] = {
    extractBNode(subj) ++
      extractBNode(obj)
  }

  def iris: Set[IRI] = {
    extractIRI(subj) ++
      extractIRI(pred) ++
      extractIRI(obj)
  }

  override def toString: String = {
    subj + " " + pred + " " + obj + " ."
  }
}

object RDFTriple {

  /**
   *  Constructor of RDFTriples from triples
   */
  def apply(triple: (RDFNode, IRI, RDFNode)) = new RDFTriple(triple._1, triple._2, triple._3)

  def apply(triple: (RDFNode, IRI, RDFNode), base: IRI) = new RDFTriple(resolve(triple._1, base),
    base.resolve(triple._2),
    resolve(triple._3, base))

  def resolve(node: RDFNode, base: IRI): RDFNode = {
    node match {
      case iri: IRI => base.resolve(iri)
      case x => x
    }
  }

  /**
   * collects BNodes in a set of triples
   */
  def collectBNodes(triples: Set[RDFTriple]): Set[BNodeId] = {
    triples.foldLeft(Set[BNodeId]())((set, triple) =>
      set ++ triple.bNodes)
  }

  /**
   * Shows a set of triples
   * TODO: Consider removing this method
   */
  def showTriples(triples: Set[RDFTriple]): String = {
    var str = new StringBuilder
    for { t <- triples } {
      str ++= (t + "\n")
    }
    str.toString()
  }

}

