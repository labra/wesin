package es.weso.parser

import scala.util.parsing.combinator.RegexParsers
import org.scalatest.FunSpec
import org.scalatest.Matchers

trait TestParser 
	extends FunSpec 
	with RegexParsers 
	with Matchers {

  def shouldParse(p:Parser[String], s : String) {
     shouldParseGeneric(p,s,s)
   }

   // Only checks if parser succeeds
   def shouldParseGen[A](p:Parser[A], s : String) {
    it("Should parse \"" + s + "\"") {
      val result = parseAll(p,s) match {
        case Success(x,_) => true 
        case NoSuccess(msg,_) => fail(msg)
      }
    }
   }

    def shouldParseGeneric[A](p:Parser[A], s : String, a : A) {
    it("Should parse \"" + s + "\"" + " and return " + a.toString) {
      val result = parseAll(p,s) match {
        case Success(x,_) => x 
        case NoSuccess(msg,_) => fail(msg)
      }
      result should be(a)
    }
   }
 
    def shouldNotParse[A](p:Parser[A], s : String) {
    it("Should not parse \"" + s + "\"") {
      val result = parseAll(p,s) match {
        case Success(x,_) => fail("Should not parse " + s + ", but parsed value " + x) 
        case NoSuccess(msg,_) => info("It didn't parse as expected. Message: " + msg)
      }
    }
   } 
    
   /** 
    *  Checks if parser succeeds
    *  @param testName name of the test
    *  @param p parser
    *  @param s input string to parse
    *  
    */
   def shouldParseNamed[A](testName: String, 
		   				   p:Parser[A], 
		   				   s : String) {
    it("Should parse " + testName) {
      val result = parseAll(p,s) match {
        case Success(x,_) => true 
        case NoSuccess(msg,_) => 
          	fail(msg + "\n" + s + 
        	     "\n-----------------\n")
      }
    }
   }

  /** 
    *  Checks if parser does not succeed (negative test)
    *  @param testName name of the test
    *  @param p parser
    *  @param s input string to parse
    *  
    */
   def shouldNotParseNamed[A](testName: String, 
		   				   p:Parser[A], 
		   				   s : String) {
    it("Should not parse " + testName) {
      val result = parseAll(p,s) match {
        case Success(x,s1) => 
          fail("String: " + s + " should not parse, but it parsed with value: " + x + " and " + s1)
        case NoSuccess(msg,_) => 
          	info("It didn't parse as expected. Message: " + msg)
      }
    }
   }

}