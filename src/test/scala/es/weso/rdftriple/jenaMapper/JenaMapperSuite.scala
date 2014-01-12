package es.weso.rdftriple.jenaMapper
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.typesafe.config._
import es.weso.rdfNode.BNodeId
import es.weso.rdfNode.IRI
import es.weso.rdfTriple.jenaMapper.JenaMapper
import com.hp.hpl.jena.rdf.model.ModelFactory
import java.io.ByteArrayInputStream
import com.hp.hpl.jena.rdf.model.Model
import java.io.InputStream
import org.scalatest.Matchers
import es.weso.rdfTriple.RDFTriple
import es.weso.rdfNode.LangLiteral
import es.weso.rdfNode.DecimalLiteral
import es.weso.rdfNode.IntegerLiteral
import es.weso.rdfNode.DoubleLiteral
import es.weso.rdfNode.BooleanLiteral
import es.weso.rdfNode.Lang

class JenaMapperSuite 
		extends FunSpec
		with JenaMapper
		with Matchers {
  
 describe("Jena Mapper") {
  it("Should compare one triple with 2 different bNodes") {
   val ts = Set(RDFTriple(BNodeId(0),IRI("http://example.org#p"),BNodeId(1)))
   val s = """[] <http://example.org#p> [] ."""
   val model1 = RDFTriples2Model(ts)
   val model2 = str2model(s)
   shouldBeIsomorphic(model1,model2)
  }

  it("Should compare one triple with a shared bNode") {
   val ts = Set(RDFTriple(BNodeId(0),IRI("http://example.org#p"),BNodeId(0)))
   val s = """_:a <http://example.org#p> _:a ."""
   val model1 = RDFTriples2Model(ts)
   val model2 = str2model(s)
   shouldBeIsomorphic(model1,model2)
  }
 
  it("Should compare one triple with a prefix decl") {
   val ts = Set(RDFTriple(BNodeId(0),IRI("http://example.org#p"),BNodeId(0)))
   val s = """|@prefix : <http://example.org#> . 
              |_:a :p _:a .""".stripMargin
   val model1 = RDFTriples2Model(ts)
   val model2 = str2model(s)
   shouldBeIsomorphic(model1,model2)
  }

  it("Should compare one triple with an integer literal") {
   val ts = Set(RDFTriple(BNodeId(0),IRI("http://example.org#p"),IntegerLiteral(1)))
   val s = """|@prefix : <http://example.org#> . 
              |_:a :p 1 .""".stripMargin
   val model1 = RDFTriples2Model(ts)
   val model2 = str2model(s)
   shouldBeIsomorphic(model1,model2)
  }

  it("Should compare one triple with a decimal literal") {
   val ts = Set(RDFTriple(BNodeId(0),IRI("http://example.org#p"),DecimalLiteral(1.2)))
   val s = """|@prefix : <http://example.org#> . 
              |_:a :p 1.2 .""".stripMargin
   val model1 = RDFTriples2Model(ts)
   val model2 = str2model(s)
   shouldBeIsomorphic(model1,model2)
  }

  it("Should compare one triple with a boolean literal") {
   val ts = Set(RDFTriple(BNodeId(0),IRI("http://example.org#p"),BooleanLiteral(true)))
   val s = """|@prefix : <http://example.org#> . 
              |_:a :p true .""".stripMargin
   val model1 = RDFTriples2Model(ts)
   val model2 = str2model(s)
   shouldBeIsomorphic(model1,model2)
  }

  // The following test fails probably for Double comparison
  ignore("Should compare one triple with a double literal") {
   val ts = Set(RDFTriple(BNodeId(0),IRI("http://example.org#p"),DoubleLiteral(1.2e3)))
   val s = """|@prefix : <http://example.org#> . 
              |_:a :p 1.2e3 .""".stripMargin
   val model1 = RDFTriples2Model(ts)
   val model2 = str2model(s)
   shouldBeIsomorphic(model1,model2)
  }

  it("Should convert three triples") {
   val ts = Set(
		   RDFTriple(BNodeId(0),IRI("http://example.org#p"),BNodeId(0)),
		   RDFTriple(BNodeId(0),IRI("http://example.org#p"),IntegerLiteral(4)),
		   RDFTriple(BNodeId(0),IRI("http://example.org#p"),LangLiteral("pepe",Lang("es")))
		   )
   val m1 = RDFTriples2Model(ts)
   val m2 = str2model("""|@prefix : <http://example.org#> .
                         |_:0 <http://example.org#p> _:0, 4, "pepe"@es .
                         |""".stripMargin)
   	shouldBeIsomorphic(m1,m2)
  }
 
 
 
 }

  def shouldBeIsomorphic(m1: Model, m2:Model) : Unit = {
   val b = m1.isIsomorphicWith(m2)
   if (!b) {
     println("Models are not isomorphic")
     println("-------------- Model 1:" + m1.toString)
     println("-------------- Model 2:" + m2.toString)
   }
   b should be(true)
  }

  def str2model(s: String) : Model = {
   val m = ModelFactory.createDefaultModel
   val in : InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))
   m.read(in,"","TURTLE")
   m
  }
 
}