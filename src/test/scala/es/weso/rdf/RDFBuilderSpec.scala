package es.weso.rdf

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.FunSpec
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes.IRI
import es.weso.rdf.nodes.StringLiteral
import es.weso.rdf.jena._
import es.weso.rdf.PREFIXES._

@RunWith(classOf[JUnitRunner])
class RDFBuilderSpec
    extends FunSpec
    with JenaBased
    with Matchers {

  describe("RDFBuilder") {
    it("should create an RDF graph with a single Triple") {
      val rdf = RDFAsJenaModel.empty
      val rdf1: RDFAsJenaModel =
        rdf.addPrefix("", "http://example.org/").
          addPrefix("a", "http://a.com/").
          addTriple(RDFTriple(rdf.qName("a:b"), rdf_type, rdf.qName(":c")))

      val m2 = str2model("""|@prefix : <http://example.org/> .
                            |@prefix a: <http://a.com/> .
                            |a:b a :c .
                            |""".stripMargin)

      shouldBeIsomorphic(rdf1.model, m2)

    }

    it("should create an RDF graph with some BNodes") {
      val rdf = RDFAsJenaModel.empty
      rdf.addPrefix("", "http://example.org/").
        addPrefix("a", "http://a.com/").
        addTriple(RDFTriple(rdf.qName("a:b"), rdf_type, rdf.qName(":c")))
      val (bn, _) = rdf.createBNode
      rdf.addTriple(RDFTriple(bn, rdf_type, rdf.qName(":c"))).
        addTriple(RDFTriple(bn, rdf_type, rdf.qName(":d")))

      val m2 = str2model("""|@prefix : <http://example.org/> .
                            |@prefix a: <http://a.com/> .
                            |a:b a :c .
                            |_:1 a :c, :d .
                            |""".stripMargin)

      shouldBeIsomorphic(rdf.model, m2)

    }

  }
}

