package es.weso.rdf.jena

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.FunSpec
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdfgraph.nodes._
import es.weso.rdf.jena._
import com.hp.hpl.jena.rdf.model.ModelFactory
import es.weso.rdf._

@RunWith(classOf[JUnitRunner])
class RDFJenaSpec
    extends FunSpec
    with JenaBased
    with Matchers {
  describe("Adding triples") {
    it("should be able to add a single triple with IRIs") {
      val emptyModel = ModelFactory.createDefaultModel
      val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      val map: Map[String, IRI] = Map("" -> IRI("http://example.org#"))
      val pm: PrefixMap = PrefixMap(map)
      rdf.addPrefixMap(pm)
      rdf.addTriples(Set(RDFTriple(IRI("http://example.org#a"), IRI("http://example.org#b"), IRI("http://example.org#c"))))
      val m2 = str2model("""|@prefix : <http://example.org#> .
                            |:a :b :c .
                            |""".stripMargin)

      shouldBeIsomorphic(rdf.model, m2)
    }
    it("should be able to add some triples with BNodes") {
      val emptyModel = ModelFactory.createDefaultModel
      val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      val map: Map[String, IRI] = Map(
        "" -> IRI("http://example.org#"), "foaf" -> IRI("http://foaf.org#")
      )
      val pm: PrefixMap = PrefixMap(map)
      rdf.addPrefixMap(pm)
      rdf.addTriples(Set(
        RDFTriple(IRI("http://example.org#a"), IRI("http://foaf.org#knows"), BNodeId(1)), RDFTriple(BNodeId(1), IRI("http://foaf.org#knows"), BNodeId(2)), RDFTriple(BNodeId(2), IRI("http://foaf.org#name"), StringLiteral("pepe"))
      ))
      val m2 = str2model("""|@prefix : <http://example.org#> .
                            |@prefix foaf: <http://foaf.org#> .
                            |:a foaf:knows _:x .
                            |_:x foaf:knows _:y .
                            |_:y foaf:name "pepe" .
                            |""".stripMargin)

      shouldBeIsomorphic(rdf.model, m2)
    }

  }

  describe("Parsing other formats") {
    it("Should be able to parse NTriples") {
      val m1 = str2model("""|@prefix : <http://example.org#> .
                          |:a :b :c .
                          |""".stripMargin)
      val str_triples = "<http://example.org#a> <http://example.org#b> <http://example.org#c> ."
      val rdf: RDFAsJenaModel = RDFAsJenaModel(ModelFactory.createDefaultModel())
      val rdf2 = rdf.parse(str_triples, "NTRIPLES").get
      val m2 = RDFAsJenaModel.extractModel(rdf2)
      shouldBeIsomorphic(m1, m2)
    }
  }
}
