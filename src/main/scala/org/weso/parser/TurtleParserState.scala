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
  val curSubjectLs : List[RDFNode],
  val curPredicateLs : List[IRI],
  val namespaces : PrefixMap ,
  val bNodeLabels : BNodeTable,
  val baseIRI: IRI
  ) {

 def addCurSubject(subj:RDFNode):TurtleParserState = 
    TurtleParserState(subj :: curSubjectLs, curPredicateLs, namespaces, bNodeLabels,baseIRI)

 def addCurPredicate(pred:IRI): TurtleParserState =
    TurtleParserState(curSubjectLs, pred :: curPredicateLs, namespaces, bNodeLabels,baseIRI)

 def curSubject : RDFNode =
   curSubjectLs head

 def curPredicate : IRI =
   curPredicateLs head 

 def newTable (table: BNodeTable) : TurtleParserState = 
   TurtleParserState(curSubjectLs,curPredicateLs,namespaces,table,baseIRI)
   
 def addPrefix(prefix: String, iri: IRI) : TurtleParserState = 
   TurtleParserState(curSubjectLs,curPredicateLs,namespaces.addPrefix(prefix, iri),bNodeLabels,baseIRI)

 def newBNode : (BNodeId,TurtleParserState) = { 
   val (id,t) = bNodeLabels.newBNode ; 
   (id,TurtleParserState(curSubjectLs,curPredicateLs,namespaces,t,baseIRI))
 }
 
 def newBase(newIRI:IRI) =
   TurtleParserState(curSubjectLs,curPredicateLs,namespaces,bNodeLabels,newIRI)

}

object TurtleParserState {
  
  def initial = TurtleParserState(List(),List(),PrefixMap.empty,BNodeTable.empty,IRI(""))
  
  def initial(baseIRI : IRI) = TurtleParserState(List(),List(),PrefixMap.empty,BNodeTable.empty,baseIRI)
  
}