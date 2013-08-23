package org.weso.parser

import scala.util.parsing.combinator.RegexParsers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import util.parsing.input.CharSequenceReader.EofCh

class StateParserSuite 
	extends RegexParsers 
	with TestParser
	with StateParser
	with W3cTokens
	with FunSpec 
	with ShouldMatchers {

  describe("repState") {
    val s = SimpleState.initial
    val parserAs = (s : SimpleState) => opt(WS) ~> repState(s,newS("A"))
    shouldParseGeneric(parserAs(s),"",(List(),SimpleState(0)))
    shouldParseGeneric(parserAs(s)," ",(List(),SimpleState(0)))
    shouldParseGeneric(parserAs(s),"A",(List(0),SimpleState(1)))
    shouldParseGeneric(parserAs(s),"AA",(List(0,1),SimpleState(2))) 
    shouldParseGeneric(parserAs(s),"A A",(List(0,1),SimpleState(2))) 
    shouldParseGeneric(parserAs(s),"AAA",(List(0,1,2),SimpleState(3))) 
  }

  describe("rep1sepState") {
    val s = SimpleState.initial
    val parserAs = (s : SimpleState) => rep1sepState(s,newS("A"),",")
    shouldParseGeneric(parserAs(s),"A,A",(List(0,1),SimpleState(2)))
    shouldParseGeneric(parserAs(s),"A",(List(0),SimpleState(1)))
    shouldNotParse(parserAs(s),"")
    
    val parserWithColon = rep1sepOptState(s,parserAs,";")
    shouldParseGeneric(parserWithColon,"A,A;A",(List(List(0,1),List(2)),SimpleState(3)))
    shouldParseGeneric(parserWithColon,"A,A;A,A",(List(List(0,1),List(2,3)),SimpleState(4)))
    
  } 



	
	/**
	 * Auxiliary class to represent a very simple state of int's
	 */
	case class SimpleState(value : Int) {
		def newState : (Int,SimpleState) = (value, SimpleState(value + 1))
	}

	object SimpleState {
		val initial : SimpleState = SimpleState(0)
	}  

	/** 
	 *  Parses a term and updates the state
	 */
	def newS(term:String)
  		  (s:SimpleState):
	  Parser[(Int,SimpleState)] = 
	    ( opt(WS) ~> 
	      acceptRegex("term("+term+")",term.r)) ^^^ 
	          { s.newState }
}


