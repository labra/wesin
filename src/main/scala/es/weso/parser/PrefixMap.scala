package es.weso.parser

import util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional
import scala.util.parsing.input._
import util.parsing.input.CharSequenceReader.EofCh
import es.weso.rdfNode._
import es.weso.rdfTriple._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.io.Codec
import scala.util.matching.Regex
import scala.collection.immutable.Map


case class PrefixMap(map: Map[String,IRI]) {

  def getIRI(prefix : String) : Option[IRI] = {
    map.get(prefix)
  }
  
  def contains(prefix: String) : Boolean = map.contains(prefix)

  def addPrefix(prefix : String, iri: IRI) : PrefixMap = {
    PrefixMap(map + (prefix -> iri))
  }

  override def toString : String = {
    "Prefix map: " + map
  }
}

object PrefixMap {
  def empty = PrefixMap(Map[String,IRI]())
  
  def addPrefix(prefix: String, iri: IRI)(pm: PrefixMap) : PrefixMap =
    pm.addPrefix(prefix,iri)

}

