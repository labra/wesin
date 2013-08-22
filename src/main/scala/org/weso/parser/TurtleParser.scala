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

trait TurtleParser extends Positional with RegexParsers {

  override val skipWhitespace = false

  def turtleDoc(implicit s: ParserState) : 
	  		Parser[(List[(RDFNode,RDFNode,RDFNode)],ParserState)] = 
     repState(s,statement) ^^ { case (lss,s) => (lss.flatten,s)
    }
  
  def statement(s:ParserState): 
	  		Parser[(List[(RDFNode,RDFNode,RDFNode)],ParserState)] = 
    opt(WS) ~> 
     ( directive(s) ^^ { case s1 => (List(),s1) }
     | triples(s) <~ token(".") 
     ) 

  def directive (s:ParserState) : Parser[ParserState] = 
    prefixDirective(s) // | baseDirective(s)
  
  def prefixDirective (s: ParserState) : Parser[ParserState] = {
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

  def triples(s:ParserState) : 
	  Parser[(List[(RDFNode,RDFNode,RDFNode)],ParserState)] = 
  ( subjPredicatesObjectList(s) ^^ { 
    case (ns,s) => (toTriples(ns),s)
   }
  // TODO: | blankNodePropertyList predicateObjectList
  )
  
  def toTriples[A](ns : (A,List[(A,List[A])])) : List[(A,A,A)] = {
    for (ps <- ns._2; y <- ps._2 ) yield (ns._1,ps._1,y)
  } 

  def subjPredicatesObjectList(s:ParserState) :
      Parser[((RDFNode,List[(RDFNode,List[RDFNode])]),ParserState)] = {
    seqState(subject, predicateObjectList)(s) ^^ {
      case (n ~ ls, s) => ((n,ls),s)
    }
  } 

  def predicateObjectList(s: ParserState) : 
	  Parser[(List[(RDFNode,List[RDFNode])],ParserState)] = 
	 rep1sepOptState(s,verbObjectList,token(";")) 

  def verbObjectList(s: ParserState) : 
	  Parser[((RDFNode,List[RDFNode]),ParserState)] = 
	    opt(WS) ~> verb(s.namespaces) ~ objectList(s) ^^ {
    case node ~ objs => ((node,objs._1),objs._2) 
  }

  def prueba(s:ParserState):Parser[Any] = 
     rep1sepOptState(s,As,token(";"))

  def As(s:ParserState) = rep1sepState(s,newT("A"),token(","))
  
  def newT(t:String)(s:ParserState):Parser[(BNodeId,ParserState)] =
    token(t) ^^^ { s.newBNode }
     
  def objectList(s: ParserState) : 
      Parser[(List[RDFNode],ParserState)] =
	    rep1sepState(s,rdf_object,token(",")) 

  def ignoreCaseToken(tk : String) : Parser[String] =
    token("(?i)"+ tk)
	    
  def token(tk: String): Parser[String] = 
    ( opt(WS) ~> tk.r <~ opt(WS)
    | failure (tk + " expected")
    )
  
  def repState[T,S](s: S, 
		  			p: S => Parser[(T,S)]
		  		   ): Parser[(List[T],S)] = 
	  rep1State(s,p) | success((List(),s))
  
  def rep1sepOptState[T,S](s : S, 
		  				p : S => Parser[(T,S)], 
		  				q : => Parser[Any]): Parser[(List[T],S)] =
    p(s) >> { s1 => repState(s1._2, arrowOptState(p,q)) ^^ {
       case (ls,s2) => (s1._1::ls.flatten,s2)
       } 
    }

  def arrowOptState[T,S](p : S => Parser[(T,S)],q: Parser[Any])
		  			    (s : S) : Parser[(Option[T],S)] =
    q ~> opt(p(s)) ^^ { 
      case None => (None,s)
      case Some((t,s1)) => (Some(t),s1)
	} 

  def rep1sepState[T,S](s : S, 
		  				p : S => Parser[(T,S)], 
		  				q : => Parser[Any]): Parser[(List[T],S)] =
    p(s) >> { s1 => repState(s1._2, arrowState(p,q)) ^^ {
       case (ls,s2) => (s1._1::ls,s2)} 
  }

  def seqState[T,U,S](p:S => Parser[(T,S)],
		  			  q:S => Parser[(U,S)])(s:S) : Parser[(T ~ U,S)] = {
    p(s) >> { s1 => q(s1._2) ^^ { case (u,s2) => (new ~(s1._1,u), s2)} }
  }

  def arrowState[T,S](p : S => Parser[(T,S)],
		  			       q: Parser[Any])(s : S) : Parser[(T,S)] =
		  			    q ~> p(s)

  def rep1State[T,S]
	   (s: S, p: S => Parser[(T,S)]): Parser[(List[T],S)] = 
    	rep1State(s, p, p)

  def rep1State[T,S](s : S,
  			         first: S => Parser[(T,S)], 
  			         p0: S => Parser[(T,S)]
  			         ): 
          Parser[(List[T],S)] = 
   Parser { in =>
    lazy val p = p0 // lazy argument
    val elems : ListBuffer[T] = new ListBuffer[T]

    def continue(s: S)
    			(in: Input): ParseResult[(List[T],S)] = {
      val p0 = p    // avoid repeatedly re-evaluating by-name parser

      @tailrec def applyp(s0:S)(in0: Input): 
    	  			ParseResult[(List[T],S)] = p0(s0)(in0) match 
    	  			{
        case Success(x, rest) => elems += x._1 ; applyp(x._2)(rest)
        case e @ Error(_, _)  => e  // still have to propagate error
        case _                => Success((elems.toList,s0), in0)
      }

      applyp(s)(in)
    }

    first(s)(in) match {
      case Success(x, rest) => elems += x._1 ; continue(x._2)(rest)
      case ns: NoSuccess    => ns
    }
  }	  	  
	   
 	  
  
  def verb (ns : PrefixMap) : Parser[RDFNode] =
      ( predicate(ns)
      | "a" ^^ { case _ => (RDFNode.rdftype) }
      )
      
  def subject(s : ParserState) :  
    Parser[(RDFNode,ParserState)] = 
      ( iri(s.namespaces) ^^ { case iri => (iri,s)}
      | BlankNode(s.bNodeLabels) ^^ { case (id,t) => (id,s.newTable(t))} 
      )

  def predicate = iri _
  
	
  def rdf_object(s: ParserState) : 
	  Parser[(RDFNode,ParserState)] = 
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
  
  lazy val IRIREF_STR  = "<([^\\u0000-\\u0020<>\\\\\"{}\\|\\^`\\\\]|" + UCHAR + ")*>"
  lazy val IRIREF : Parser[IRI] = 
    acceptRegex("IRIREF",IRIREF_STR.r) ^^ {
      case x => val rex = "<(.*)>".r
                val rex(cleanIRI) = x  // removes < and >
                IRI(cleanIRI)
    }
  

  def PNAME_NS_STR	= "(" + PN_PREFIX + ")?:"
  
  def acceptRegex(name : String, r : Regex) : Parser[String] = 
    ( r | failure(name + " expected with regular expression " + r))

  def PNAME_NS_Parser : Parser[String] = acceptRegex("PNAME_NS",PNAME_NS_STR.r) 

  def PNAME_NS(prefixMap: PrefixMap): Parser[IRI] = {
   PNAME_NS_Parser ^? 
        ({ case prefix 
  		    if (prefixMap.contains(prefix)) => { 
  		  	    prefixMap.getIRI(prefix).get
  		    }
  		 },
  		 { case prefix => 
  		   "Prefix " + prefix + 
  		   " not found in Namespce map: " + 
  		   prefixMap
  		 })
  }
  
  lazy val PNAME_LN_STR = PNAME_NS_STR + PN_LOCAL
  
  def PNAME_LN (prefixMap: PrefixMap): Parser[IRI]	= {
   PNAME_NS(prefixMap) ~ PN_LOCAL.r ^^
  		{ case prefix ~ local => 
  		  	{
  		  	  println("Prefix: " + prefix + ". Local: " + local)
  		  	  RDFNode.qNameIRI(prefix, unscapeReservedChars(local))
  		  	}
  		}
  }
  
  lazy val BLANK_NODE_LABEL_STR = "_:(" + PN_CHARS_U + "|[0-9])(("+ PN_CHARS + "|\\.)*" + PN_CHARS + ")?"
  
  def BLANK_NODE_LABEL(bNodeTable:BNodeTable) : Parser[(BNodeId,BNodeTable)] = 
    	BLANK_NODE_LABEL_STR.r ^^ { 
    	  s => bNodeTable.getOrAddBNode(removeBNodePrefix(s)) 
      	}
  
  lazy val LANGTAG		= "@" ~> "[a-zA-Z]+(-[a-zA-Z0-9]+)*".r ^^ Lang 

  lazy val INTEGER: Parser[Literal]  = "[+-]?[0-9]+".r ^^ 
  				{ x => IntegerLiteral(str2Int(x)) }
  
  lazy val DECIMAL: Parser[Literal]	 = "[+-]?[0-9]*\\.[0-9]+".r ^^ 
		  		{ x => DecimalLiteral(str2Decimal(x))}

  lazy val DOUBLE : Parser[Literal]   = 
		  ("[+-]?([0-9]+\\.[0-9]*" + EXPONENT + "|\\.[0-9]+" + EXPONENT + "|[0-9]+" + EXPONENT + ")").r ^^ 
		  		{ x => DoubleLiteral(str2Double(x)) }
  
  lazy val EXPONENT		= "[eE][+-]?[0-9]+"

  lazy val STRING_LITERAL_QUOTE_STR : String    = "\"([^\\u0022\\u005C\\u000A\\u000D]|" + ECHAR + "|" + UCHAR + ")*\""
  lazy val STRING_LITERAL_SINGLE_QUOTE_STR      = "'([^\\u0027\\u005C\\u000A\\u000D]|" + ECHAR + "|" + UCHAR + ")*'"
  lazy val STRING_LITERAL_LONG_SINGLE_QUOTE_STR = "'''(('|'')?[^']|" + ECHAR + "|" + UCHAR + ")*'''"
  lazy val STRING_LITERAL_LONG_QUOTE_STR		= "\"\"\"((\"|\"\")?[^\"]|" + ECHAR + "|" + UCHAR + ")*\"\"\""
  
  
  lazy val STRING_LITERAL_QUOTE : Parser[String] = STRING_LITERAL_QUOTE_STR.r  ^^ {
   x => removeQuotes(unscape(x),"\"",1)
  }

  lazy val STRING_LITERAL_SINGLE_QUOTE 			 = STRING_LITERAL_SINGLE_QUOTE_STR.r ^^ {
   x => removeQuotes(unscape(x),"\'",1)
  }
  lazy val STRING_LITERAL_LONG_SINGLE_QUOTE      = STRING_LITERAL_LONG_SINGLE_QUOTE_STR.r ^^ 
    { x => removeQuotes(unscape(x),"\'",3) }
  lazy val STRING_LITERAL_LONG_QUOTE 	         = STRING_LITERAL_LONG_QUOTE_STR.r ^^ 
    { x => removeQuotes(unscape(x),"\"",3) }
  

  
  lazy val UCHAR_Parser : Parser[Char] = UCHAR.r ^^ { x => UCHAR2char(x) }

  lazy val UCHAR 		= "\\\\u" + HEX + HEX + HEX + HEX + "|" + "\\\\U" + HEX + HEX + HEX + HEX + HEX + HEX
  
  lazy val ECHAR_Parser : Parser[Char] = ECHAR.r ^^ { x => ECHAR2char(x) }
  lazy val ECHAR 		= "\\\\[tbnrf\"]" 
  lazy val WS_STR 			= """\u0020|\u0009|\u000D|\u000A"""
    
  lazy val WS = rep ( WS_STR.r 
		  			| "#" ~ rep(chrExcept(EofCh, '\n') )
		  			)

  def chrExcept(cs: Char*) = elem("", ch => (cs forall (ch != _)))
  

  lazy val ANON_STR = "\\[(" + WS_STR + ")*\\]"  

  def ANON(bNodeTable: BNodeTable) : Parser[(BNodeId,BNodeTable)] = 
    ANON_STR.r ^^ { _ => bNodeTable.newBNode 
    } 

  lazy val PN_CHARS_BASE_Parser : Parser[Char] = PN_CHARS_BASE.r ^^ { x => str2Char(x) }

  lazy val PN_CHARS_BASE =
 	"""[a-zA-Z\u00C0-\u00D6\u00D8-\u00F6""" +
 	"""\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF""" +
 	"""\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF""" +
 	"""\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD""" + 
   	"""\x{10000}-\x{EFFFF}]""" 

 lazy val PN_CHARS_U    = PN_CHARS_BASE + "|_"
 
 lazy val PN_CHARS		= PN_CHARS_U + """|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040]""" 
 
 lazy val PLX			= PERCENT + "|" + PN_LOCAL_ESC
 
 lazy val PN_PREFIX     = PN_CHARS_BASE + "((" + PN_CHARS + "|\\.)*" + PN_CHARS + ")?" 
 
 lazy val PN_LOCAL		= "(" + PN_CHARS_U + "|:|[0-9]|" + PLX + ")((" + PN_CHARS + "|\\.|:|" + PLX + ")*(" + PN_CHARS + "|:|" + PLX + "))?"     
 
 lazy val PERCENT 		= "%" + HEX + HEX 
 
 lazy val HEX 			= """[0-9A-Fa-f]"""  
 
 lazy val PN_LOCAL_ESC 	= """[\\][_~\.\-!$&'\(\)*+,;=/?#@%]"""
 

 def UCHAR2char (uchar:String) : Char = {
    val rex = """\\[uU](.*)""".r
    uchar match {
      case rex(x) => Integer.parseInt(x,16).toChar
      case _ => throw new Exception("Internal Error: cannot convert uchar " + uchar + " to " + rex.pattern)
    }
 }
  
 def ECHAR2char(echar:String) : Char = {
    echar match {
      case "\\t" => '\t'
      case "\\b" => '\b' 
      case "\\n" => '\n'
      case "\\r" => '\r'
      case "\\f" => '\f'
      case "\\\"" => '\"'
      case _ => throw new Exception("Internal Error: cannot convert ECHAR " + echar + " to character")
    }
 }

 /**
  * remove quotes from a quoted string
  * 
  * @param s input string
  * @param quote Type of quotes, may be simple or double
  * @param Number of quotes, normally 1 or 3
  * @return the new string
  * 
  */
 def removeQuotes(s : String, quote: String, number: Int) : String = {
    // Note: (?s) enables multiline matching
    val rex = ("(?s)"+quote + "{" + number.toString + "}(.*)" + quote + "{"+number.toString + "}").r
    val rex(newS) = s
    newS 
  }

 def removeBNodePrefix(s : String) : String = {
   val rex = "\\_:(.*)".r
   val rex(newS) = s
   newS
 }
  
 def str2Double(s: String) : Double = s.toDouble 
 def str2Decimal(s: String) : BigDecimal = BigDecimal(s)
 def str2Int(s: String) : Integer = Integer.parseInt(s)

 // The following code does the unscape traversing the list recursively
 def str2Char(str:String) : Char = {
    str.charAt(0)
 }

 def hex2Char (s : List[Char]) : Char= {
   try {
      Integer.parseInt(s.mkString,16).toChar
   } catch {
     case e : Throwable => throw new 
    		 Exception("Internal Error 'hex2Char': cannot convert from unicode chars. Value: " + 
    				 s.mkString + "\n " + "Exception raised: " + e.toString)
   }
 }

 def unscape(s:String) : String = {
   unscapeList(s.toList).mkString
 }
 
 def unscapeList(s:List[Char]) : List[Char] = {
   s match { 
     case '\\' :: 'u' :: a :: b :: c :: d :: rs => hex2Char(a :: b :: c :: d :: Nil) :: unscapeList(rs)
     case '\\' :: 'U' :: a :: b :: c :: d :: e :: f :: rs => hex2Char(a :: b :: c :: d :: e :: f :: Nil) :: unscapeList(rs)
     case '\\' :: 't' :: rs => '\t' :: unscapeList(rs)
     case '\\' :: 'b' :: rs => '\b' :: unscapeList(rs)
     case '\\' :: 'n' :: rs => '\n' :: unscapeList(rs)
     case '\\' :: 'r' :: rs => '\r' :: unscapeList(rs)
     case '\\' :: 'f' :: rs => '\f' :: unscapeList(rs)
     case '\\' :: '\"' :: rs => '\"' :: unscapeList(rs)
     case '\\' :: '\'' :: rs => '\'' :: unscapeList(rs)
     case '\\' :: '\\' :: rs => '\\' :: unscapeList(rs)
     case c :: rs => c :: unscapeList(rs)
     case Nil => Nil
   }
 }

 def unscapeReservedChars(s:String) : String = 
   unscapeReservedCharsList(s.toList).mkString
   
 def unscapeReservedCharsList(s:List[Char]) : List[Char] = {
   s match {
     case '\\' :: c :: rs if "~.-!$&'()*+,;=/?#@%_".contains(c) => c :: unscapeReservedCharsList(rs)
     case c :: rs => c :: unscapeReservedCharsList(rs)
     case Nil => Nil
   }
 }
 // Alternative way to unscape using regular expressions....
 def hex2str(s : String) : String = {
   Integer.parseInt(s.mkString,16).toChar.toString
 }

 def unscapeUnicode4(s: String): String = {
    val rex = """\\u(\p{XDigit}{4})""".r
    rex.replaceAllIn(s, m => Regex quoteReplacement hex2str(m group 1))
 }
 
 def unscapeUnicode6(s: String): String = {
    val rex = """\\U(\p{XDigit}{6})""".r
    rex.replaceAllIn(s, m => Regex quoteReplacement hex2str(m group 1))
 }

 def unscapeCtrl(s:String) : String = {
    val ctrl = """\\[bnrtf\\"]""".r
    ctrl.replaceAllIn(s, m => Regex quoteReplacement ctrl2str(m group 1))
 }

 def ctrl2str(s : String) : String = {
  s match {
    case "\\b" => "\b"
    case "\\t" => "\t"
    case "\\n" => "\n"
    case "\\r" => "\r"
    case "\\f" => "\f"
    case "\\\"" => "\""
    case "\\\'" => "\'"
    case "\\\\" => "\\"
    case s => s      
  }
 }

 def unscape2(x:String) : String = 
     (unscapeUnicode4 _ andThen unscapeUnicode6 andThen unscapeCtrl)(x)
 //------------------------------------
     
 
}
 
object TurtleParser extends TurtleParser 

