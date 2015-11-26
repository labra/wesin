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
import es.weso.rdf.PREFIXES._

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
        RDFTriple(IRI("http://example.org#a"), IRI("http://foaf.org#knows"), BNodeId("b" + 1)), RDFTriple(BNodeId("b" + 1), IRI("http://foaf.org#knows"), BNodeId("b" + 2)), RDFTriple(BNodeId("b" + 2), IRI("http://foaf.org#name"), StringLiteral("pepe"))
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

  describe("Querying RDF graphs") {
    it("Should be able to get objects of some type") {
      val str = """|@prefix : <http://example.org#> .
                   |:a a :C ; :p 1 .
                   |:b a :C, :D .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.parse(str, "TURTLE").get
      val typeC = IRI("http://example.org#C")
      val triples = rdf.triplesWithType(typeC)
      val a = IRI("http://example.org#a")
      val b = IRI("http://example.org#b")
      val t1 = RDFTriple(a, rdf_type, typeC)
      val t2 = RDFTriple(b, rdf_type, typeC)
      triples should be(Set(t1, t2))
    }

    it("Should be able to get subjects") {
      val str = """|@prefix : <http://example.org#> .
                   |:a a :C ; :p 1 .
                   |:b a :C, :D .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.parse(str, "TURTLE").get
      val a = IRI("http://example.org#a")
      val b = IRI("http://example.org#b")
      val p = IRI("http://example.org#p")
      val typeC = IRI("http://example.org#C")
      val triples = rdf.triplesWithSubject(a)

      val t1 = RDFTriple(a, rdf_type, typeC)
      val t2 = RDFTriple(a, p, IntegerLiteral(1))
      triples should be(Set(t1, t2))
    }

    it("Should be able to get subjects with xsd:date") {
      val str = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :date "25/10/2015"^^xsd:date .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.parse(str, "TURTLE").get
      val a = IRI("http://example.org#a")
      val date = IRI("http://example.org#date")
      val value = DatatypeLiteral("25/10/2015", IRI("http://www.w3.org/2001/XMLSchema#date"))
      val triples = rdf.triplesWithSubject(a)
      val t1 = RDFTriple(a, date, value)
      triples should be(Set(t1))
    }

    it("Should be able to get subjects with xsd:integer") {
      val str = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :age 15 .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.parse(str, "TURTLE").get
      val a = IRI("http://example.org#a")
      val age = IRI("http://example.org#age")
      val value = IntegerLiteral(15)
      val triples = rdf.triplesWithSubject(a)
      val t1 = RDFTriple(a, age, value)
      triples should be(Set(t1))
    }

    it("Should be able to get subjects with datatype :xxx") {
      val str = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :age "15"^^:xxx .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.parse(str, "TURTLE").get
      val a = IRI("http://example.org#a")
      val age = IRI("http://example.org#age")
      val value = DatatypeLiteral("15", IRI("http://example.org#xxx"))
      val triples = rdf.triplesWithSubject(a)
      val t1 = RDFTriple(a, age, value)
      triples should be(Set(t1))
    }

    it("Should be able to get subjects with lang literal") {
      val str = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :age "hi"@en .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.parse(str, "TURTLE").get
      val a = IRI("http://example.org#a")
      val age = IRI("http://example.org#age")
      val value = LangLiteral("hi", Lang("en"))
      val triples = rdf.triplesWithSubject(a)
      val t1 = RDFTriple(a, age, value)
      triples should be(Set(t1))
    }

  }

}
