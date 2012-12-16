package org.weso.parser

import util.parsing.combinator.JavaTokenParsers

trait ArithParser extends JavaTokenParsers {
	def expr: Parser[Any] = term ~ rep("+" ~ term | "-" ~ term)
	def term = factor ~ rep("*" ~ factor | "/" ~ factor)
	def factor = floatingPointNumber | "(" ~ expr ~ ")"
}

object TurtleParser extends ArithParser {
	def main(args: Array[String]) {
	 
	  println("Expression? ")
	  val input = Console.readLine()
	  println("output: " + parseAll(expr, input))
	}
}
