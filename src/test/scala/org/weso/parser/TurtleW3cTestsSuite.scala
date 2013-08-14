package org.weso.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scala.io.Source

import org.weso.rdfNode._
import org.weso.rdfTriple._
import scala.util.parsing.input.CharArrayReader

class TurtleW3cTestsSuite extends TurtleParser with FunSpec with ShouldMatchers {

  describe("Turtle Lexer") {
   val parser=TurtleParser

   describe("RDFLiteral") {
     val p = parser.RDFLiteral
     shouldParseGen(p,"\"John\"")
	 shouldParseGen(p,"\"123\"^^<xsd:integer>")
	 shouldParseGen(p,"\"\"\"John Doe\"\"\"")
	 shouldParseGen(p,"one@es")
	 shouldParseGen(p,"one")
	 shouldParseGen(p,"one two")
	 shouldParseGen(p,".")
	 shouldNotParse(p,".")
   }

   describe("BooleanLiteral") {
     val p = parser.BooleanLiteral
     shouldParse(p,"true")
	 shouldParse(p,"false")
	 shouldNotParse(p,"tres")
	 shouldNotParse(p,"one")
   }

	 
	 describe("string") {
     val p = parser.string
     shouldParse(p,"\"hi\"")
	 shouldParse(p,"'Hi'")
	 shouldParse(p,"'''Hi'John'''")
	 shouldNotParse(p,"3.2")
   }

   describe("iri") {
     val p = parser.iri
     shouldParse(p,":a")
	 shouldParse(p,"a:b")
	 shouldParse(p,"p:")
	 shouldParse(p,"<http://a.com>")
	 shouldParse(p,"<a.com>")
	 shouldNotParse(p,"3.2")
   }

   describe("PrefixedName") {
     val p = parser.PrefixedName
     shouldParse(p,":a")
	 shouldParse(p,"a:b")
	 shouldParse(p,"p:")
	 shouldNotParse(p,"<a>")
   }

   describe("BlankNode") {
     val p = parser.BlankNode
     shouldParse(p,"_:1")
	 shouldParse(p,"_:a")
	 shouldParse(p,"[]")
	 shouldNotParse(p,"<a>")
   }

   describe("IRIREF") {
     val p = IRIREF.r
     shouldParse(p,"<>")
	 shouldParse(p,"<hi>")
	 shouldParse(p,"<http://pepe.com>")
	 shouldParse(p,"<urn://kiko-uno>")
	 shouldNotParse(p,"<a>.")
	 shouldNotParse(p,"<a\"b>")
	 shouldNotParse(p,"<a{a}b>")
   }

   describe("PNAME_NS") {
     val p = PNAME_NS.r
     shouldParse(p,"a:")
	 shouldParse(p,":")
	 shouldParse(p,"año:")
	 shouldNotParse(p,"a:b")
   }

   describe("PNAME_LN") {
     val p = PNAME_LN.r
     shouldParse(p,"a:b")
	 shouldParse(p,"a:aa")
	 shouldParse(p,"a:año")
	 shouldNotParse(p,"a:año ")
   }

   describe("BLANK_NODE_LABEL") {
     val p = BLANK_NODE_LABEL.r
	 shouldParse(p,"_:1")
	 shouldParse(p,"_:aa")
	 shouldParse(p,"_:año")
// fails?	 shouldParse(p,"_:1.2")
	 shouldParse(p,"_:1.2a")
	 shouldParse(p,"_:1.a")
	 shouldNotParse(p,"_:1 a .")
   }

   describe("LANGTAG") {
     val p = LANGTAG.r
	 shouldParse(p,"@es")
	 shouldParse(p,"@es-am")
	 shouldParse(p,"@am-am2-23")
	 shouldNotParse(p,"es")
	 shouldNotParse(p,"@12")
	 shouldNotParse(p,"@año")
   }

   describe("INTEGER") {
     val p = INTEGER.r
	 shouldParse(p,"2")
	 shouldParse(p,"-3")
	 shouldParse(p,"-33")
	 shouldNotParse(p,"3.4")
	 shouldNotParse(p,"334.")
	 shouldNotParse(p,"-3.34E+98")
   }

	 describe("DECIMAL") {
     val p = DECIMAL.r
	 shouldParse(p,"2.34")
	 shouldParse(p,"-3.0")
	 shouldParse(p,"-3.3")
	 shouldNotParse(p,"3")
	 shouldNotParse(p,"334")
	 shouldNotParse(p,"-3.34E+98")
	 shouldNotParse(p,"-334")
   }

   describe("DOUBLE") {
     val p = DOUBLE.r
	 shouldParse(p,"2.34e9876")
	 shouldParse(p,"-3E+98")
	 shouldParse(p,"-3.34E+98")
	 shouldNotParse(p,"-3.34EE+98")
	 shouldNotParse(p,"-3.34E +98")
   }

   describe("EXPONENT") {
     val p = EXPONENT.r
	 shouldParse(p,"e9876")
	 shouldParse(p,"E+98")
	 shouldParse(p,"E-98")
	 shouldParse(p,"e-98")
	 shouldNotParse(p,"e++")
	 shouldNotParse(p,"e+a")
	 shouldNotParse(p,"E+-98")
   }

   describe("STRING_LITERAL_QUOTE") {
     val p = STRING_LITERAL_QUOTE.r
	 shouldParse(p,"\"Hello\"")
	 shouldParse(p,"\"\\u0123\t\\u00AF\"")
	 shouldNotParse(p,"\"Ho\"la\"")
   }

   describe("STRING_LITERAL_SINGLE_QUOTE") {
     val p = STRING_LITERAL_SINGLE_QUOTE.r
	 shouldParse(p,"'Hello'")
	 shouldParse(p,"'\\u0123'")
	 shouldNotParse(p,"'Ho'la'")
   }

   describe("STRING_LITERAL_LONG_SINGLE_QUOTE") {
     val p = STRING_LITERAL_LONG_SINGLE_QUOTE.r
	 shouldParse(p,"'''Hello'''")
	 shouldParse(p,"'''\\u0123'''")
	 shouldParse(p,"'''H e l l o . '''")
	 shouldParse(p,"'''Hi 'John' . '''")
	 shouldParse(p,"'''Hi'John'''")
	 shouldParse(p,"'''Hi \"John\" . '''")
	 shouldParse(p,"'''Hi \"\"John\"\" . '''")
	 shouldNotParse(p,"\"\"\"Hi \"\"\"John\"\" . \"\"\"")
   }

   describe("STRING_LITERAL_LONG_QUOTE") {
     val p = STRING_LITERAL_LONG_QUOTE.r
	 shouldParse(p,"\"\"\"Hello\"\"\"")
	 shouldParse(p,"\"\"\"\\u0123\"\"\"")
	 shouldParse(p,"\"\"\"H e l l o . \"\"\"")
	 shouldParse(p,"\"\"\"Hi \"John\" . \"\"\"")
	 shouldParse(p,"\"\"\"Hi \'John\' . \"\"\"")
	 shouldParse(p,"\"\"\"Hi \"\"John\"\" . \"\"\"")
	 shouldNotParse(p,"\"\"\"Hi \"\"\"John\"\" . \"\"\"")
	 
   }
   
   describe("UCHAR") {
     val p = UCHAR.r
	 shouldParse(p,"\\u0123")
	 shouldParse(p,"\\u01AF")
	 shouldParse(p,"\\U0123AD")
	 shouldNotParse(p,"\\x0123AD")
	 shouldNotParse(p,"\\u0123AD")
	 shouldNotParse(p,"\\U0123")
	 shouldNotParse(p,"\\u012H")
   }

   describe("ECHAR") {
     val p = ECHAR.r
	 shouldParse(p,"\\t")
	 shouldParse(p,"\\b")
	 shouldParse(p,"\\n")
	 shouldParse(p,"\\r")
	 shouldParse(p,"\\f")
	 shouldParse(p,"\\\"")
	 shouldNotParse(p,"a")
   }

   
   describe("WS") {
     val p = WS.r
	 shouldParse(p," ")
	 shouldNotParse(p,"a")
   }

   describe("ANON") {
     val p = ANON.r
	 shouldParse(p,"[]")
	 shouldParse(p,"[ ]")
	 shouldParse(p,"[ \t]")
	 shouldParse(p,"[ \n]")
	 shouldNotParse(p,"[a]")
   }

   describe("PN_CHARS_BASE") {
     val p = PN_CHARS_BASE.r
	 shouldParse(p,"A")
	 shouldParse(p,"Ñ")
	 shouldParse(p,"\u2088")
	 shouldNotParse(p, "\u2001")
   }

   describe("PN_CHARS") {
     val p = PN_CHARS.r
	 shouldParse(p,"A")
	 shouldParse(p,"-")
	 shouldParse(p,"7")
	 shouldParse(p,"\u00B7")
	 shouldNotParse(p, ":")
   }


   describe("PN_PREFIX") {
     val p = PN_PREFIX.r
	 shouldParse(p,"A")
	 shouldParse(p,"a.o")
	 shouldParse(p,"a..o")
	 shouldParse(p,"a.ñ.o")
	 shouldParse(p,"a.x.o")
//	 shouldParse(p,"a-1")
//	 shouldParse(p,"a--1")
	 shouldParse(p,"año")
     shouldParse(p,"AA")
     shouldParse(p, "b.a")
	 shouldNotParse(p, "b:a")
   }

   
   describe("PN_LOCAL") {
     val p = PN_LOCAL.r
	 shouldParse(p,"Año")
	 shouldParse(p,":a")
	 shouldParse(p,":89")
	 shouldParse(p,":año_88")
	 shouldParse(p,":año:88")
	 shouldNotParse(p,":año88 .")
	 shouldNotParse(p,":año88,")
   }

   describe("PLX") {
     val p = PLX.r
	 shouldParse(p,"%12")
	 shouldParse(p,"\\-")
   }

   describe("PERCENT") {
     val p = PERCENT.r
	 shouldParse(p,"%12")
	 shouldParse(p,"%9F")
	 shouldParse(p,"%FF")
	 shouldNotParse(p, "AAA")
	 shouldNotParse(p, "%%")
	 shouldNotParse(p, "%GH")
   }

   
   describe("PN_LOCAL_ESC") {
     val p = PN_LOCAL_ESC.r
     shouldParse(p,"\\-")
     shouldParse(p,"\\!")
     shouldNotParse(p,"a")
   }

   def shouldParse(p:TurtleParser.Parser[String], s : String) {
     shouldParseGeneric(p,s,s)
   }

   def shouldParseGen[A](p:TurtleParser.Parser[A], s : String) {
    it("Should parse \"" + s + "\"") {
      parser.parseAll(p,s).successful === true
    }
   }

   def shouldParseGeneric[A](p:TurtleParser.Parser[A], s : String, a : A) {
    it("Should parse \"" + s + "\"") {
      parser.parseAll(p,s).get === a
    }
   }
 
   def shouldNotParse[A](p:TurtleParser.Parser[A], s : String) {
    it("Should not parse \"" + s + "\"") {
      parser.parseAll(p,s) === parser.NoSuccess
    }
   }
  
  }
}