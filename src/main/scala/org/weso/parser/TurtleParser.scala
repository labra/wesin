package org.weso.parser

import util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional
import scala.util.parsing.input._
import util.parsing.input.CharSequenceReader.EofCh

import org.weso.rdfNode._
import org.weso.rdfTriple._

trait TurtleParser extends Positional with RegexParsers {
 override val skipWhitespace = false

 lazy val PN_CHARS_BASE_STR =
 	"[a-zA-Z]|[\\u00C0-\\u00D6]|[\\u00D8-\\u00F6]|[\\u00F8-\\u02FF]|[\\u0370-\\u037D]|[\\u037F-\\u1FFF]|" + 
   	"[\\u200C-\\u200D]|[\\u2070-\\u218F]|[\\u2C00-\\u2FEF]|[\\u3001-\\uD7FF]|[\\uF900-\\uFDCF]|[\\uFDF0-\\uFFFD]|" + 
   	"[\\x{10000}-\\x{EFFFF}]"

 lazy val PN_CHARS_BASE				= PN_CHARS_BASE_STR.r
 lazy val PN_CHARS_U				= PN_CHARS_BASE | "_"
 lazy val PN_CHARS					= PN_CHARS_U | "-" | "[0-9]".r | "\\u00B7" | 
                                      "[\\u0300-\u036F]".r | "[\\u203F-\\u2040]".r
 
 lazy val PN_PREFIX					= PN_CHARS_BASE ~ opt ( rep(PN_CHARS | ".") ~ PN_CHARS) ^^ 
   { case c1 ~ None => c1
     case c1 ~ Some(ls) => c1 + ls
   } 
 
 lazy val PLX						= PERCENT | PN_LOCAL_ESC
 lazy val PERCENT : Parser[String]	= "%" ~> HEX ~ HEX ^^ { case h1 ~ h2 => "%" + h1 + h2 } // To convert: Integer.parseInt(h1 + h2,16) } 
 lazy val HEX 						= """[0-9A-Fa-f]""".r
 lazy val PN_LOCAL_ESC 				= """[\\][_~\.\-!$&'\(\)*+,;=/?#@%]""".r 
 


}

object TurtleParser extends TurtleParser 