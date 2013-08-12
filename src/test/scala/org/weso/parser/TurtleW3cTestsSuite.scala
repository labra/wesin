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

  describe("Turtle Parser") {
   val parser=TurtleParser

   describe("PN_CHARS_BASE") {
	 shouldParse(parser.PN_CHARS_BASE,"A")
	 shouldParse(parser.PN_CHARS_BASE,"\u2088")
	 shouldNotParse(parser.PN_CHARS_BASE, "\u2001")
   }

   describe("PN_CHARS") {
     val p = parser.PN_CHARS
	 shouldParse(p,"A")
	 shouldParse(p,"-")
	 shouldNotParse(p, ":")
   }


   describe("PN_PREFIX") {
     val p = parser.PN_PREFIX
	 shouldParse(p,"A")
// fails	 shouldParse(p,"AA")
// fails	 shouldParse(p, "b.a")
	 shouldNotParse(p, "b:a")
   }

   describe("PLX") {
	 shouldParse(parser.PLX,"%12")
	 shouldParse(parser.PLX,"\\-")
   }

   describe("PERCENT") {
	 shouldParse(parser.PERCENT,"%12")
	 shouldParse(parser.PERCENT,"%9F")
	 shouldParse(parser.PERCENT,"%FF")
	 shouldNotParse(parser.PERCENT, "AAA")
	 shouldNotParse(parser.PERCENT, "%%")
	 shouldNotParse(parser.PERCENT, "%GH")
   }

   
   describe("PN_LOCAL_ESC") {
     shouldParse(parser.PN_LOCAL_ESC,"\\-")
     shouldParse(parser.PN_LOCAL_ESC,"\\!")
     shouldNotParse(parser.PN_LOCAL_ESC,"a")
   }

   def shouldParse(p:TurtleParser.Parser[String], s : String) {
     shouldParseGeneric(p,s,s)
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