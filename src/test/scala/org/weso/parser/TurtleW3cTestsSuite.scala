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



class TurtleW3cTestsSuite 
		extends TurtleParser with FunSpec with ShouldMatchers {

  describe("Turtle Parser using W3c tests") {
   val parser=TurtleParser

   describe("parse manifest file") {
     implicit val s = ParserState.initial
     val p = parser.turtleDoc

     val input = Source.fromURL(getClass.getResource("/test2.nt")).mkString

     shouldParseGen(p,input)
   }

   
   def shouldParse(p:TurtleParser.Parser[String], s : String) {
     shouldParseGeneric(p,s,s)
   }

   // Only checks if parser succeeds
   def shouldParseGen[A](p:TurtleParser.Parser[A], s : String) {
    it("Should parse \"" + s + "\"") {
      val result = parser.parseAll(p,s) match {
        case parser.Success(x,_) => true 
        case parser.NoSuccess(msg,_) => fail(msg)
      }
    }
   }

    def shouldParseGeneric[A](p:TurtleParser.Parser[A], s : String, a : A) {
    it("Should parse \"" + s + "\"" + " and return " + a.toString) {
      val result = parser.parseAll(p,s) match {
        case parser.Success(x,_) => x 
        case parser.NoSuccess(msg,_) => fail(msg)
      }
      result should be(a)
    }
   }
 
    def shouldParseRDF[A](p:TurtleParser.Parser[A], s : String, a : A) {
    it("Should parse \"" + s + "\"" + " and return " + a.toString) {
      val result = parser.parseAll(p,s) match {
        case parser.Success((x,_),_) => x 
        case parser.NoSuccess(msg,_) => fail(msg)
      }
      result should be(a)
    }
   }
 
    def shouldNotParse[A](p:TurtleParser.Parser[A], s : String) {
    it("Should not parse \"" + s + "\"") {
      val result = parser.parseAll(p,s) match {
        case parser.Success(x,_) => fail("Should not parse " + s + ", but parsed value " + x) 
        case parser.NoSuccess(msg,_) => success(msg)
      }
    }
   }
  
  }
}