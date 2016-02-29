package es.weso.parser

import org.scalatest.FunSpec
import es.weso.rdf.nodes._
import org.scalatest.Matchers
import es.weso.rdf.PrefixMap

class BNodeTableSuite extends FunSpec with Matchers {

  describe("BNode Table") {

    it("getBNodeId should return nothing from empty table") {
      val table = BNodeTable.empty
      table.getBNodeId("pepe") should be(None)
    }

    it("getBNodeName should return nothing from empty table") {
      val table = BNodeTable.empty
      val id9 = BNodeId("b" + 9)
      table.getBNodeName(id9) should be(None)
    }

    it("getOrAddBNode should return a value from a table with 3 values") {
      val table = BNodeTable.empty
      val (_, table1) = table.getOrAddBNode("id0")
      val (_, table2) = table1.getOrAddBNode("id1")
      val (_, table3) = table2.getOrAddBNode("id2")
      table3.getOrAddBNode("id1")._1 should be(BNodeId("b" + 1))
    }

    it("getBNodeName should return a value from a table with 3 values") {
      val table = BNodeTable.empty
      val (id0, table1) = table.getOrAddBNode("id0")
      val (id1, table2) = table1.getOrAddBNode("id1")
      val (id2, table3) = table2.getOrAddBNode("id2")
      table3.getBNodeName(id1) should be(Some("id1"))
    }

    it("compare tables") {
      val table1 = BNodeTable.empty
      val table2 = BNodeTable.empty
      val (_, table11) = table1.getOrAddBNode("id0")
      val (_, table22) = table2.getOrAddBNode("id0")
      table11 should be(table22)
    }
  }
}