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



class BNodeTableSuite extends BNodeTable with FunSpec with ShouldMatchers {

  describe("BNode Table") {

   it("getBNodeId should return nothing from empty table") {
     val table = new BNodeTable
     table.getBNodeId("pepe") should be(None)
   }

   it("getBNodeName should return nothing from empty table") {
     val table = new BNodeTable
     val id9 = BNodeId(9)
     table.getBNodeName(id9) should be(None)
   }
  
   it("getOrAddBNode should return a value from a table with 3 values") {
     val table = new BNodeTable
     table.newBNode
     table.getOrAddBNode("id0")
     table.getOrAddBNode("id1")
     table.getOrAddBNode("id2")
     table.getOrAddBNode("id1") should be(BNodeId(2))
   }

   it("getBNodeName should return a value from a table with 3 values") {
     val table = new BNodeTable
     table.newBNode
     table.getOrAddBNode("id0")
     table.getOrAddBNode("id1")
     table.getOrAddBNode("id2")
     table.getBNodeName(BNodeId(2)) should be(Some("id1"))
   }

  }
}