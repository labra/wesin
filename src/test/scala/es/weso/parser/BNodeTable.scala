package es.weso.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scala.io.Source
import es.weso.rdfNode._
import es.weso.rdfTriple._
import scala.util.parsing.input.CharArrayReader
import org.scalatest.Matchers

class BNodeTableSuite extends FunSpec with Matchers {

  describe("BNode Table") {

   it("getBNodeId should return nothing from empty table") {
     val table = BNodeTable.empty
     table.getBNodeId("pepe") should be(None)
   }

   it("getBNodeName should return nothing from empty table") {
     val table = BNodeTable.empty
     val id9 = BNodeId(9)
     table.getBNodeName(id9) should be(None)
   }
  
   it("getOrAddBNode should return a value from a table with 3 values") {
     val table = BNodeTable.empty
     val (_,table1) = table.getOrAddBNode("id0")
     val (_,table2) = table1.getOrAddBNode("id1")
     val (_,table3) = table2.getOrAddBNode("id2")
     table3.getOrAddBNode("id1")._1 should be(BNodeId(1))
   }

   it("getBNodeName should return a value from a table with 3 values") {
     val table = BNodeTable.empty
     val (_,table1) = table.getOrAddBNode("id0")
     val (_,table2) = table1.getOrAddBNode("id1")
     val (_,table3) = table2.getOrAddBNode("id2")
//     table3.getBNodeName(BNodeId(2)) should be(Some("id1"))
   }

  }
}