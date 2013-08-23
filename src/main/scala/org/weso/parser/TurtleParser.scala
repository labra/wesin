package org.weso.parser

import util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import util.parsing.input.CharSequenceReader.EofCh
import org.weso.rdfNode._
import org.weso.rdfTriple._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.io.Codec
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

trait TurtleParser 
	extends Positional 
	with RegexParsers 
	with StateParser 
	with W3cTokens {

  def turtleDoc(implicit s: TurtleParserState) : 
	  		Parser[(List[RDFTriple],TurtleParserState)] = 
     opt(WS) ~> repState(s,statement) ^^ 
     	{ case (lss,s) => (lss.flatten,s) }
     
    
  
  def statement(s:TurtleParserState): 
	  		Parser[(List[RDFTriple],TurtleParserState)] = 
     ( directive(s) <~ opt(WS) ^^ { case s1 => (List(),s1) }
     | triples(s) <~ token(".")
     ) 

  def directive (s:TurtleParserState) : Parser[TurtleParserState] = 
    prefixDirective(s) // | baseDirective(s)
  
  def prefixDirective (s: TurtleParserState) : Parser[TurtleParserState] = {
    (SPARQLPrefix | prefixId ) ^^ {
      case (prefix,iri) => s.addPrefix(prefix, iri)
    }
  }

  def SPARQLPrefix : Parser[(String,IRI)] = {
    ignoreCaseToken("PREFIX") ~> PNAME_NS_Parser ~ (WS ~> IRIREF) ^^ {
      case s ~ iri => (s,iri)
    }
  }
  
  def prefixId : Parser[(String,IRI)] = {
    token("@prefix") ~> PNAME_NS_Parser ~ (WS ~> IRIREF) ^^ {
      case s ~ iri => (s,iri)
    }
  }

  def triples(s:TurtleParserState) : 
	  Parser[(List[RDFTriple],TurtleParserState)] = 
  ( subjPredicatesObjectList(s) ^^ { 
    case (ns,s) => (toTriples(ns).map(t => RDFTriple(t)),s)
   }
  // TODO: | blankNodePropertyList predicateObjectList
  | opt(WS) ^^^ { (List(),s)}
  )
  
  def toTriples[A,B,C](ns : (A,List[(B,List[C])])) : List[(A,B,C)] = {
    for (ps <- ns._2; y <- ps._2 ) yield (ns._1,ps._1,y)
  } 

  def subjPredicatesObjectList(s:TurtleParserState) :
      Parser[((RDFNode,List[(IRI,List[RDFNode])]),TurtleParserState)] = {
    seqState(subject, predicateObjectList)(s) ^^ {
      case (n ~ ls, s) => ((n,ls),s)
    }
  } 

  def predicateObjectList(s: TurtleParserState) : 
	  Parser[(List[(IRI,List[RDFNode])],TurtleParserState)] = 
	 rep1sepOptState(s,verbObjectList,token(";")) 

  def verbObjectList(s: TurtleParserState) : 
	  Parser[((IRI,List[RDFNode]),TurtleParserState)] = 
	    opt(WS) ~> verb(s.namespaces) ~ objectList(s) ^^ {
    case node ~ objs => ((node,objs._1),objs._2) 
  }

     
  def objectList(s: TurtleParserState) : 
      Parser[(List[RDFNode],TurtleParserState)] =
	    rep1sepState(s,rdf_object,token(",")) 

  def ignoreCaseToken(tk : String) : Parser[String] =
    token("(?i)"+ tk)
	    
  def token(tk: String): Parser[String] = 
    ( opt(WS) ~> tk.r <~ opt(WS)
    | failure (tk + " expected")
    )
  
  def verb (ns : PrefixMap) : Parser[IRI] =
      ( predicate(ns)
      | "a" ^^ { case _ => (RDFNode.rdftype) }
      )
      
  def subject(s : TurtleParserState) :  
    Parser[(RDFNode,TurtleParserState)] = 
       ( iri(s.namespaces) ^^ { case iri => (iri,s)}
       | BlankNode(s.bNodeLabels) ^^ { case (id,t) => (id,s.newTable(t))} 
       )

  def predicate = iri _
  
	
  def rdf_object(s: TurtleParserState) : 
	  Parser[(RDFNode,TurtleParserState)] = 
	opt(WS) ~>
        ( iri(s.namespaces) ^^ { case iri => (iri,s)}
	    | BlankNode(s.bNodeLabels) ^^ { case (id,table) => (id,s.newTable(table))} 
	    | literal(s.namespaces) ^^ { case l => (l,s) }
	    ) <~ opt(WS)
  
  
  def literal(prefixMap : PrefixMap) : Parser[Literal] = 
    	(  
    	  NumericLiteral 
    	| RDFLiteral(prefixMap)
    	| BooleanLiteral
    	) 
  def blankNodePropertyList = ???
  def collection = ???
  
  lazy val NumericLiteral : Parser[Literal] =  
    ( DOUBLE | DECIMAL | INTEGER ) 
    
  def RDFLiteral(prefixMap: PrefixMap) = 
    string ~ opt(LANGTAG | "^^" ~> iri(prefixMap)) ^^ {
    case str ~ None => StringLiteral(str) 
    case str ~ Some(Lang(l)) => LangLiteral(str,Lang(l)) 
    case str ~ Some(IRI(iri)) => DatatypeLiteral(str,IRI(iri))
  }
  
  lazy val BooleanLiteral : Parser[Literal] = 
  		( "true"  ^^ { _ => RDFNode.trueLiteral  } 
  		| "false" ^^ { _ => RDFNode.falseLiteral }
  		)
  		
  lazy val string : Parser[String] = opt(WS) ~> (
    	STRING_LITERAL_LONG_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | 
  		STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE 
  		)
  					
  def iri(prefixMap: PrefixMap) = 
    	( IRIREF   
  		| PrefixedName(prefixMap) 
  		| failure("iri expected")
  		)
  		
  def PrefixedName(prefixMap: PrefixMap) : Parser[IRI] = 
      ( PNAME_LN(prefixMap) 
      | PNAME_NS(prefixMap) 
      )
      
  def BlankNode(bNodeTable: BNodeTable) : Parser[(BNodeId,BNodeTable)] = 
    	( BLANK_NODE_LABEL(bNodeTable) 
    	| ANON(bNodeTable) 
    	| failure("Blank Node expected")
    	)
  
}
 

