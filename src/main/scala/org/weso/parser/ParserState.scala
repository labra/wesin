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

case class ParserState (
  val curSubjectLs : List[RDFNode],
  val curPredicateLs : List[IRI],
  val namespaces : PrefixMap ,
  val bNodeLabels : BNodeTable) {

 def addCurSubject(subj:RDFNode):ParserState = 
    ParserState(subj :: curSubjectLs, curPredicateLs, namespaces, bNodeLabels)

 def addCurPredicate(pred:IRI): ParserState =
    ParserState(curSubjectLs, pred :: curPredicateLs, namespaces, bNodeLabels)

 def curSubject : RDFNode =
   curSubjectLs head

 def curPredicate : IRI =
   curPredicateLs head 

 def newTable (table: BNodeTable) : ParserState = 
   ParserState(curSubjectLs,curPredicateLs,namespaces,table)
   
 def addPrefix(prefix: String, iri: IRI) : ParserState = 
   ParserState(curSubjectLs,curPredicateLs,namespaces.addPrefix(prefix, iri),bNodeLabels)

 def newBNode : (BNodeId,ParserState) = { 
   val (id,t) = bNodeLabels.newBNode ; 
   (id,ParserState(curSubjectLs,curPredicateLs,namespaces,t))
 }
 
}

object ParserState {
  def initial = ParserState(List(),List(),PrefixMap.empty,BNodeTable.empty)
  
    

    
}