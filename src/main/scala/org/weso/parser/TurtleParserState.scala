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
import scala.collection.immutable.Map
import scala.language.postfixOps

case class TurtleParserState (
  val triples : List[RDFTriple],
  val namespaces : PrefixMap ,
  val bNodeLabels : BNodeTable,
  val baseIRI: IRI
  ) {

  def addTriple (t : RDFTriple) : TurtleParserState = {
    TurtleParserState(t :: triples, namespaces, bNodeLabels, baseIRI)
  }

  def addTriples (ts : List[RDFTriple]) : TurtleParserState = {
    TurtleParserState(ts ++ triples, namespaces, bNodeLabels, baseIRI)
  }

  def retrieveTriples: (List[RDFTriple], TurtleParserState) = {
   (triples, TurtleParserState(List(), namespaces, bNodeLabels, baseIRI))
  }

 def newTable (table: BNodeTable) : TurtleParserState = 
   TurtleParserState(triples,namespaces,table,baseIRI)
   
 def addPrefix(prefix: String, iri: IRI) : TurtleParserState = 
   TurtleParserState(triples,namespaces.addPrefix(prefix, iri),bNodeLabels,baseIRI)

 def newBNode : (BNodeId,TurtleParserState) = { 
   val (id,t) = bNodeLabels.newBNode ; 
   (id,TurtleParserState(triples,namespaces,t,baseIRI))
 }
 
 def newBase(newIRI:IRI) =
   TurtleParserState(triples,namespaces,bNodeLabels,newIRI)

}

object TurtleParserState {
  
  def initial : TurtleParserState = initial(IRI(""))
  def initial(baseIRI : IRI) = TurtleParserState(List(),PrefixMap.empty,BNodeTable.empty,baseIRI)
  
}