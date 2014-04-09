package es.weso.parser

import util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import util.parsing.input.CharSequenceReader.EofCh
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph.nodes._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.io.Codec
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph.nodes._
import scala.util.Try


trait TurtleParser 
	extends Positional 
	with RegexParsers 
	with StateParser 
	with W3cTokens {

  case class ResultParser[A,S](t: A, s: S) extends Positional
  
  def turtleDoc(implicit s: TurtleParserState) : 
	  		Parser[ResultParser[Set[RDFTriple],TurtleParserState]] = 
     positioned ( opt(WS) ~> repState(s,statement) ^^ 
     	{ case (lss,s) => ResultParser(lss.flatten.toSet,s) }
     )
  
/*  def turtleDoc(implicit s: TurtleParserState) : 
	  		Parser[(Set[RDFTriple],TurtleParserState)] = 
     opt(WS) ~> repState(s,statement) ^^ 
     	{ case (lss,s) => (lss.flatten.toSet,s) }
*/     
     
  def statement(s:TurtleParserState): 
	  		Parser[(RDFTriples,TurtleParserState)] = 
     ( directive(s) <~ opt(WS) ^^ { case s1 => (List(),s1) }
     | triples(s) <~ token(".")
     ) 

  def directive (s:TurtleParserState) : Parser[TurtleParserState] = 
    ( prefixDirective(s) 
    | baseDirective(s)
    )
  
  def baseDirective (s: TurtleParserState) : Parser[TurtleParserState] = {
    (SPARQLBase | baseId ) ^^ {
      case (iri) => s.newBase(s.baseIRI.resolve(iri))
    }
  }

  def SPARQLBase : Parser[(IRI)] = 
    ignoreCaseToken("BASE") ~> (WS ~> IRIREF) 
  

  def baseId : Parser[IRI] = 
    token("@base") ~> (WS ~> IRIREF) <~ token(".")
  

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
    token("@prefix") ~> PNAME_NS_Parser ~ (WS ~> IRIREF) <~ token(".") ^^ {
      case s ~ iri => (s,iri)
    }
  }

  def triples(s:TurtleParserState) : 
	  Parser[(List[RDFTriple],TurtleParserState)] = 
  ( subjPredicatesObjectList(s) ^^ { 
    case (ns,s1) => {
      val (collectedTriples,s2) = s1.retrieveTriples
      (collectedTriples ++ toTriples(ns).map(t => RDFTriple(t,s.baseIRI)),s2)
     }
    }
  | seqState(blankNodePropertyList, optState(predicateObjectList))(s) ^^ {
    case ((bnode ~ None),s1) => {
      s1.retrieveTriples
    }
    case (bnode ~ Some(ps),s1) => {
       val (collectedTriples,s2) = s1.retrieveTriples
       (collectedTriples ++ toTriples((bnode,ps)).map(t => RDFTriple(t,s.baseIRI)),s2)
     }
    }
  | failure("Expected triple")
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
      
  type RDFTriples = List[RDFTriple]
  
  def subject(s : TurtleParserState) :  
    Parser[(RDFNode,TurtleParserState)] = 
       ( iri(s.namespaces) ^^ { case iri => (iri,s)}
       | BlankNode(s.bNodeLabels) ^^ { case (id,t) => (id,s.newTable(t))} 
       | collection(s)
       )

  def predicate = iri _
  
	
  def rdf_object(s: TurtleParserState) : 
	  Parser[(RDFNode,TurtleParserState)] = 
	opt(WS) ~>
        ( iri(s.namespaces) ^^ { case iri => (iri,s)}
	    | BlankNode(s.bNodeLabels) ^^ { case (id,table) => (id,s.newTable(table))} 
	    | collection(s)
        | blankNodePropertyList(s)
	    | literal(s.namespaces) ^^ { case l => (l,s) }
	    ) <~ opt(WS)
  
  
  def literal(prefixMap : PrefixMap) : Parser[Literal] = 
    	(  
    	  NumericLiteral 
    	| RDFLiteral(prefixMap)
    	| BooleanLiteral
    	) 

  def blankNodePropertyList(s: TurtleParserState) : Parser[(RDFNode,TurtleParserState)]= 
    "[" ~> predicateObjectList(s) <~ opt(WS) <~ "]" ^^ {
      case (ps,s1) => {
        val (bNode,s2) = s1.newBNode
        val s3 = s2.addTriples(toTriples((bNode,ps)).map(t => RDFTriple(t,s2.baseIRI)))
        (bNode,s3) 
      }
    }

  def collection(s:TurtleParserState): Parser[(RDFNode,TurtleParserState)] = 
    "(" ~> repState(s,rdf_object) <~ opt(WS) <~ ")" ^^ { 
       case (ls,s1) => {
         mkCollection(ls,s1)
       }
    }
   
  // TODO: Make this method tail recursive
  // TODO: Consider using a state monad 
  def mkCollection(ls : List[RDFNode], s: TurtleParserState): (RDFNode,TurtleParserState) = {
    ls match {
     case Nil => (RDFNode.rdfnil,s)
     case obj :: rs => {
       val (bNodeRs,s1) = mkCollection(rs,s) // Recursive call
       val (bNode,	s2) = s1.newBNode
       // Check: We assume that the baseURI for RDF triples generated in collection does not change
       val s3 = s2.addTriple(RDFTriple((bNode,RDFNode.rdffirst,obj),s2.baseIRI))
       (bNode, s3.addTriple(RDFTriple((bNode,RDFNode.rdfrest,bNodeRs),s3.baseIRI)))
     }
   }
  }
  
  lazy val NumericLiteral : Parser[Literal] =  
    ( DOUBLE | DECIMAL | INTEGER ) 
    
  def RDFLiteral(prefixMap: PrefixMap) = 
    string ~ opt(LANGTAG | "^^" ~> iri(prefixMap)) ^^ {
    case str ~ None => StringLiteral(str) 
    case str ~ Some(Lang(l)) => LangLiteral(str,Lang(l)) 
    case str ~ Some(i : IRI) => DatatypeLiteral(str,i)
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
 
object TurtleParser extends TurtleParser {
  
  /**
   * Parse a string with a base IRI
   * @param s: input string
   * @param baseIRI: Initial Base IRI
   * @return Left(rs) = list of triples successfully parsed
   *         Right(msg) = Error msg
   */
  def parse(cs:CharSequence, baseIRI: IRI = IRI("")) : Try[(Set[RDFTriple],PrefixMap)] = {
    try {
     parseAll(turtleDoc(TurtleParserState.initial.newBase(baseIRI)),new CharSequenceReader(cs)) match {
      case Success(ResultParser(x,s1),_) => util.Success((x,s1.namespaces))
      case NoSuccess(msg,_) 		    => util.Failure(new Exception(msg))
     }
    } catch {
      case e: Exception => util.Failure(e)
    }
  }

}

