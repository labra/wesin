package es.weso.parser

import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional
import scala.util.parsing.input._
import util.parsing.input.CharSequenceReader.EofCh
import scala.language.postfixOps


import es.weso.rdfgraph.statements._
import es.weso.rdfgraph.nodes._

/**
 * NTriples Parser
 * http://www.w3.org/TR/rdf-testcases/#ntriples
 *
 * @author labra
 * @since 18/11/2012
 */

trait NTriplesParser extends Positional with RegexParsers {
  val bNodesMap = scala.collection.mutable.Map.empty[String,BNodeId]
  
  override protected val whiteSpace = """(\s|\t)+""".r
  override val skipWhitespace = false

  
  def lazyrep[T] (p: Parser[Stream[T]]): Parser[Stream[T]] = (
    p ~ lazyrep(p) ^^ { case hd~tl => hd ++ tl }
    | success(Stream.empty)
  )

  def ntripleDoc : Parser[Stream[RDFTriple]] = {
    bNodesMap.clear()
    lazyrep(line) 
  }
  
  def line: Parser[Stream[RDFTriple]] =
    (comment | triple | eoln) ^^ {
      case Some(t) => Stream(t)
      case None => Stream.Empty
    }
  
  def triple : Parser[Option[RDFTriple]] = 
     opt(ws) ~> (subj <~ ws) ~ (pred <~ ws) ~ (obj <~ opt(ws)) <~ "." <~ opt(ws) ^^ 
       { case s ~ p ~ o => Some(RDFTriple(s,p,o)) } 
 
  def comment : Parser[Option[RDFTriple]] = 
    opt(ws) ~> """#[^\n\r]*""".r ~ eoln ^^ { case _ => None }

  
  def subj : Parser[RDFNode] = ( uriref | nodeID )
  def pred : Parser[IRI] = uriref
  def obj : Parser[RDFNode] = (uriref | nodeID | literal )
  
  def uriref : Parser[IRI] = "<" ~> absoluteURI <~ ">" ^^ {
    case uri => IRI(uri)
  }

  def nodeID : Parser[BNodeId] = "_:" ~> name ^^ 
    {(name) => 
     { bNodesMap.getOrElse(name, 
                      { val v = BNodeId(bNodesMap.size)
                      	bNodesMap.update(name,v);
                        v 
                      }) 
     }
    }
  def literal : Parser[Literal] = datatypeLiteral | langLiteral
  
  def langLiteral : Parser[LangLiteral] = string ~ opt("@" ~> language) ^^ 
  		  { case str ~ Some(lang) => LangLiteral(str,lang)
  		  	case str ~ None       => LangLiteral(str,Lang(""))
  		  } 
  
  def datatypeLiteral : Parser[DatatypeLiteral] = string ~ "^^" ~ uriref ^^ 
		  { case str ~ "^^" ~ uri => DatatypeLiteral(str,uri) }
  
  def language : Parser[Lang] = """[a-z]+(-[a-z0-9]+)*""".r ^^ Lang
  
  def ws = """(\t|\s)+""".r
  
  def eof = """\z""".r
  
  def eoln = """\n|\r|\n\r""".r ^^ { case _ => None }
  def tab = """\t""".r 
  def string : Parser[String] = "\"" ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ "\"" ^^ { _ mkString "" }  
  
  def name: Parser[String] = """[A-Za-z][A-Za-z0-9]*""".r 
  def absoluteURI : Parser[String] = rep(charURI | chrExcept('>', '\n', EofCh) ) ^^ { _ mkString "" }

  def charURI = elem("URI char", 
      (ch) => ch != '<' && ch != '>' 
        				&& ch > 0x32 // Not allowed control and space chars in URIs
        				&& ch <= 0x7F // Only ASCII (chars < 127) in URIs 
        				) 
        				
  def character = elem("character", (ch) => ch >= 20 && ch <= 126)

  def charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
    |'\\' ~ '\\' ^^^ "\\"
    |'\\' ~ '/'  ^^^ "/"
    |'\\' ~ 'b'  ^^^ "\b"
    |'\\' ~ 'f'  ^^^ "\f"
    |'\\' ~ 'n'  ^^^ "\n"
    |'\\' ~ 'r'  ^^^ "\r"
    |'\\' ~ 't'  ^^^ "\t"
    |'\\' ~> 'u' ~> unicodeBlock)

  private def unicodeBlock = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
    case a ~ b ~ c ~ d =>
      new String(Array(Integer.parseInt(List(a, b, c, d) mkString "", 16)), 0, 1)
  }
  
  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  def hexDigit = elem("hex digit", hexDigits.contains(_))

  def chrExcept(cs: Char*) = elem("", ch => (cs forall (ch !=)))
}
 
object NTriplesParser extends NTriplesParser 