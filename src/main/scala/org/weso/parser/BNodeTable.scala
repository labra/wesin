package org.weso.parser

import util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional
import scala.util.parsing.input._
import util.parsing.input.CharSequenceReader.EofCh
import org.weso.rdfNode._
import org.weso.rdfTriple._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.io.Codec
import scala.util.matching.Regex
import scala.collection.mutable.Map

class BNodeTable {
  type BNodeName = String
  private val bNodeName = Map[BNodeId,Option[BNodeName]]()
  private val nameBNode = Map[BNodeName,BNodeId]()
  private var nodes : Int = 0
  
  def newBNode : BNodeId = {
    nodes = nodes + 1
    BNodeId(nodes)
  }

  def getOrAddBNode(idName : String) : BNodeId = {
    nameBNode.get(idName) match {
      case None          => {
        val id = BNodeId(nodes)
        nodes = nodes + 1
        bNodeName += (id -> Some(idName))
        nameBNode += (idName -> id)
        id
      } 
      case Some(bNodeId) => bNodeId
    }
  }

  def getBNodeId(name : String) : Option[BNodeId] = {
    nameBNode.get(name)
  }

  def getBNodeName(id : BNodeId) : Option[BNodeName] = {
    bNodeName.get(id).getOrElse(None)
  }

  def clear() : Unit = {
    nodes = 0
    bNodeName.clear
    nameBNode.clear
  }
}

