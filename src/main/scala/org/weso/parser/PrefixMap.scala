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


class PrefixMap {
  private val prefixMap = Map[String,IRI]()
  
  def getIRI(prefix : String) : Option[IRI] = {
    prefixMap.get(prefix)
  }
  
  def contains(prefix: String) : Boolean = prefixMap.contains(prefix)

  def addPrefix(prefix : String, iri: IRI) : Unit = {
    prefixMap += (prefix -> iri)
  }

  def clear():Unit = {
    prefixMap.clear
  }

  override def toString : String = {
    "Prefix map: " + prefixMap
  }
}

