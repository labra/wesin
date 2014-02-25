package es.weso.parser

import util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional
import scala.util.parsing.input._
import util.parsing.input.CharSequenceReader.EofCh
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.io.Codec
import scala.util.matching.Regex
import scala.collection.immutable.Map
import es.weso.rdfgraph.nodes.BNodeId

case class BNodeTable(bNodeName : Map[BNodeId,Option[String]],nameBNode : Map[String,BNodeId],nodes : Int = 0) {
  
  def newBNode : (BNodeId,BNodeTable) = 
    (BNodeId(nodes), BNodeTable(bNodeName,nameBNode,nodes + 1))

  def getOrAddBNode(idName : String) : (BNodeId,BNodeTable) = {
    nameBNode.get(idName) match {
      case None          => {
        val id = BNodeId(nodes)
        (id,BNodeTable(bNodeName + (id -> Some(idName)), 
        			   nameBNode + (idName -> id),
        			   nodes + 1))
      } 
      case Some(bNodeId) => (bNodeId,this)
    }
  }

  def getBNodeId(name : String) : Option[BNodeId] = {
    nameBNode.get(name)
  }

  def getBNodeName(id : BNodeId) : Option[String] = {
    bNodeName.get(id).getOrElse(None)
  }

/*  def clear() : Unit = {
    nodes = 0
    bNodeName.clear
    nameBNode.clear
  } */
  
  override def toString: String = {
    "Nodes: " + nodes + ", bNodeName: " + bNodeName.toString + ", nameBNode: " + nameBNode
  }

}

object BNodeTable {
  def empty : BNodeTable = BNodeTable(Map[BNodeId,Option[String]](),Map[String,BNodeId](),0)
}