package es.weso.parser

import scala.util.parsing.combinator.RegexParsers
import org.scalatest.FunSpec
import util.parsing.input.CharSequenceReader.EofCh
import org.scalatest.Matchers

class StateParserSuite 
	extends FunSpec 
	with RegexParsers 
	with TestParser
	with StateParser
	with W3cTokens
	with Matchers {

  describe("OR_AND_with_chainl1") {
    class Expr
    case class Or(e1: Expr, e2: Expr) extends Expr
    case class And(e1: Expr, e2: Expr) extends Expr
    case class B(n: Integer) extends Expr

    def expr = or
	def or: Parser[Expr] = chainl1(and, "|" ^^^ Or)
	def and: Parser[Expr] = chainl1(unary, "," ^^^ And)
	def unary: Parser[Expr] = arc | "(" ~> or <~ ")"
    def arc : Parser[Expr] = "A" ^^ { case _ => B(0) }

    shouldParseGeneric(expr,"A",B(0))
    shouldParseGeneric(expr,"A,A",And(B(0),B(0)))
    shouldParseGeneric(expr,"(A),A",And(B(0),B(0)))
    shouldParseGeneric(expr,"((A)),A",And(B(0),B(0)))
    shouldParseGeneric(expr,"((A),A)",And(B(0),B(0)))
   
  }

  describe("ORAND_no State") {
    class Expr
    case class Or(e1: Expr, e2: Expr) extends Expr
    case class And(e1: Expr, e2: Expr) extends Expr
    case class B(n: Integer) extends Expr

    def expr: Parser[Expr] = OrExpr
    def OrExpr: Parser[Expr] = 
      AndExpr ~ rep("|" ~> OrExpr) ^^ 
        { case p => (ls2Expr(Or, p._1, p._2))} 

    
    def AndExpr: Parser[Expr] =
      UnaryExpr ~ rep("," ~> AndExpr) ^^ 
        { case p => (ls2Expr(And, p._1, p._2))}

    def ls2Expr (
        build:(Expr,Expr) => Expr, 
        initial: Expr, 
        ls: Seq[Expr]): Expr =
       ls.foldLeft(initial)(build)
    
    def UnaryExpr: Parser[Expr] = 
      ( "(" ~> expr <~ ")"
      | arc
      )
      
    def arc: Parser[Expr] = "A" ^^ { case a => B(0) }

    shouldParseGeneric(expr,"A",B(0))
    shouldParseGeneric(expr,"A,A",And(B(0),B(0)))
    shouldParseGeneric(expr,"(A),A",And(B(0),B(0)))
    shouldParseGeneric(expr,"((A)),A",And(B(0),B(0)))
    shouldParseGeneric(expr,"((A),A)",And(B(0),B(0)))

  }
  
  describe("ORAND") {
    class Expr
    case class Or(e1: Expr, e2: Expr) extends Expr
    case class And(e1: Expr, e2: Expr) extends Expr
    case class B(n: Integer) extends Expr
    
    val s = SimpleState.initial
    
    def expr(s: SimpleState): Parser[(Expr,SimpleState)] = 
      OrExpr(s)
    
    def OrExpr(s: SimpleState): Parser[(Expr,SimpleState)] = 
     // Todo: implement this with chainl1 
     seqState(AndExpr,repS(arrowState(OrExpr,"|")))(s) ^^ 
        { case (p,s1) => (ls2Expr(Or, p._1, p._2),s1)} 

    
    def AndExpr(s: SimpleState): Parser[(Expr,SimpleState)] =
      seqState(UnaryExpr,repS(arrowState(AndExpr,",")))(s) ^^ 
        { case (p,s1) => (ls2Expr(And, p._1, p._2),s1)}

    def ls2Expr (
        build:(Expr,Expr) => Expr, 
        initial: Expr, 
        ls: Seq[Expr]): Expr =
       ls.foldLeft(initial)(build)
    
    def UnaryExpr(s: SimpleState): Parser[(Expr,SimpleState)] = 
      ( arc(s) 
      | "(" ~> expr(s) <~ ")"
      )
      
    def arc(s:SimpleState): Parser[(Expr,SimpleState)] =
      newS("A")(s) ^^ { case (a,s1) => (B(a),s1) }
    
    shouldParseGeneric(expr(s),"A",(B(0),SimpleState(1)))
    shouldParseGeneric(expr(s),"A,A",(And(B(0),B(1)),SimpleState(2)))
    shouldParseGeneric(expr(s),"(A),A",(And(B(0),B(1)),SimpleState(2)))
    shouldParseGeneric(expr(s),"((A)),A",(And(B(0),B(1)),SimpleState(2)))
    shouldParseGeneric(expr(s),"((A),A)",(And(B(0),B(1)),SimpleState(2)))
    shouldParseGeneric(expr(s),"A,(A)",(And(B(0),B(1)),SimpleState(2)))    
    shouldParseGeneric(expr(s),"(A,(A))",(And(B(0),B(1)),SimpleState(2)))    
    shouldParseGeneric(expr(s),"(A)",(B(0),SimpleState(1)))
    shouldParseGeneric(expr(s),"(A,A)",(And(B(0),B(1)),SimpleState(2)))
    shouldParseGeneric(expr(s),"(A|A)",(Or(B(0),B(1)),SimpleState(2)))
    shouldParseGeneric(expr(s),"(A|A,A)",(Or(B(0),And(B(1),B(2))),SimpleState(3)))
    shouldParseGeneric(expr(s),"(A|(A,A))",(Or(B(0),And(B(1),B(2))),SimpleState(3)))
    shouldParseGeneric(expr(s),"((A|A),A)",(And(Or(B(0),B(1)),B(2)),SimpleState(3)))
    shouldParseGeneric(expr(s),"(A,(A|A))",(And(B(0),Or(B(1),B(2))),SimpleState(3)))
  }
  
  
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

  describe("Grammar DTs (similar to turtleDoc)") {
    val s = SimpleState.initial
    val D = newS("D")_
    val Ts = (s: SimpleState) => repState(s,newS("T"))
    val Stmt = (s: SimpleState) => D(s) | (Ts(s) <~ ".")
    val Doc = (s: SimpleState) => repState(s, Stmt)
    shouldParseGeneric(Doc(s),"DD",(List(0,1),SimpleState(2)))
    shouldParseGeneric(Doc(s),"D",(List(0),SimpleState(1)))
    shouldParseGeneric(Doc(s),"T.",(List(List(0)),SimpleState(1)))
    shouldParseGeneric(Doc(s),"DT.",(List(0,List(1)),SimpleState(2)))
    shouldParseGeneric(Doc(s),"DTT.",(List(0,List(1,2)),SimpleState(3)))
    shouldParseGeneric(Doc(s),"DTTT.",(List(0,List(1,2,3)),SimpleState(4)))
    shouldParseGeneric(Doc(s),"DT.T.",(List(0,List(1),List(2)),SimpleState(3)))
    shouldParseGeneric(Doc(s),"D.T.",(List(0,List(),List(1)),SimpleState(2)))
    shouldParseGeneric(Doc(s),"",(List(),SimpleState(0)))
    shouldNotParse(Doc(s),"A")
    shouldNotParse(Doc(s),"DA")
    shouldNotParse(Doc(s),"DT.A")
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


