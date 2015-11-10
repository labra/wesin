package es.weso.rdf

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.FunSpec
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdfgraph.nodes.IRI
import es.weso.rdfgraph.nodes.StringLiteral

@RunWith(classOf[JUnitRunner])
class RDFSpec extends FunSpec
    with Matchers {
  describe("RDFTriples") {
    it("should be able to get IRIs") {
      val rdf: RDFReader =
        RDFTriples(triples = Set(RDFTriple(IRI("a"), IRI("p"), StringLiteral("hi"))), pm = PrefixMap.empty)
      rdf.iris should be(Set(IRI("a"), IRI("p")))
    }

    it("should return IRIs in objects") {
      val rdf: RDFReader =
        RDFTriples(triples = Set(RDFTriple(IRI("a"), IRI("p"), IRI("b"))), pm = PrefixMap.empty)
      rdf.iris should be(Set(IRI("a"), IRI("p"), IRI("b")))
    }

    it("should return IRIs repeated") {
      val rdf: RDFReader =
        RDFTriples(triples = Set(RDFTriple(IRI("a"), IRI("p"), IRI("x")), RDFTriple(IRI("a"), IRI("q"), IRI("x"))), pm = PrefixMap.empty)
      rdf.iris should be(Set(IRI("a"), IRI("p"), IRI("q"), IRI("x")))
    }

  }
}
