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

trait TurtleParser extends Positional with RegexParsers {

  override val skipWhitespace = false


//  lazy val turtleDoc = PN_PREFIX

  lazy val RDFLiteral = string ~ opt(LANGTAG.r | "^^" ~ iri) 
  
  lazy val BooleanLiteral = "true" | "false" 
  lazy val string = STRING_LITERAL_LONG_QUOTE.r | STRING_LITERAL_LONG_SINGLE_QUOTE.r | 
  					STRING_LITERAL_QUOTE.r | STRING_LITERAL_SINGLE_QUOTE.r | 
  					failure("string expected")
  					
  lazy val iri = IRIREF.r | PrefixedName | failure("iri expected")
  lazy val PrefixedName = PNAME_LN.r | PNAME_NS.r | failure("Prefixed Name expected")
  lazy val BlankNode	= BLANK_NODE_LABEL.r | ANON.r | failure("Blank Node expected")
  
  lazy val IRIREF		= "<([^\\u0000-\\u0020<>\\\\\"{}\\|\\^`\\\\]|" + UCHAR + ")*>"
  lazy val PNAME_NS		= "(" + PN_PREFIX + ")?:"
  lazy val PNAME_LN 	= PNAME_NS + PN_LOCAL
  lazy val BLANK_NODE_LABEL = "_:(" + PN_CHARS_U + "|[0-9])(("+ PN_CHARS + "|\\.)*" + PN_CHARS + ")?"
  lazy val LANGTAG		= "@[a-zA-Z]+(-[a-zA-Z0-9]+)*"
  lazy val INTEGER 		= "[+-]?[0-9]+"
  lazy val DECIMAL		= "[+-]?[0-9]*\\.[0-9]+"
  lazy val DOUBLE		= "[+-]?([0-9]+\\.[0-9]*" + EXPONENT + "|\\.[0-9]+" + EXPONENT + "|[0-9]+" + EXPONENT + ")"
  lazy val EXPONENT		= "[eE][+-]?[0-9]+"
  
  lazy val STRING_LITERAL_QUOTE 
  						= "\"([^\\u0027\\u005C\\u000A\\u000D]|" + ECHAR + "|" + UCHAR + ")*\""  

  lazy val STRING_LITERAL_SINGLE_QUOTE 
  						= "'([^\\u0027\\u005C\\u000A\\u000D]|" + ECHAR + "|" + UCHAR + ")*'"  

  lazy val STRING_LITERAL_LONG_SINGLE_QUOTE 
  						= "'''(('|'')?[^']|" + ECHAR + "|" + UCHAR + ")*'''"  

  lazy val STRING_LITERAL_LONG_QUOTE 
  						= "\"\"\"((\"|\"\")?[^\"]|" + ECHAR + "|" + UCHAR + ")*\"\"\""  
  lazy val UCHAR 		= "\\\\u" + HEX + HEX + HEX + HEX + "|" + "\\\\U" + HEX + HEX + HEX + HEX + HEX + HEX
  lazy val ECHAR 		= "\\\\[tbnrf\"]" 
  lazy val WS 			= """\u0020|\u0009|\u000D|\u000A"""
  lazy val ANON 		= "\\[(" + WS + ")*\\]"  
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
 

}

object TurtleParser extends TurtleParser 

