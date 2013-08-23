package org.weso.rdftriple.jenaMapper
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.weso.rdfTriple._
import com.typesafe.config._
import org.weso.rdfNode.BNodeId
import org.weso.rdfNode.IRI
import org.weso.rdfTriple.jenaMapper.JenaMapper
import org.weso.rdfNode._

class JenaMapperSuite 
		extends JenaMapper
		with FunSpec 
		with ShouldMatchers {
  
 describe("Jena Mapper") {
  it("Should convert one triple") {
   val ts = List(RDFTriple(BNodeId(0),IRI("http://example.org#p"),BNodeId(0)))
   val m = RDFTriples2Model(ts)
   m.write(System.out)
  }

 
  it("Should convert three triples") {
   val ts = List(
		   RDFTriple(BNodeId(0),IRI("http://example.org#p"),BNodeId(0)),
		   RDFTriple(BNodeId(0),IRI("http://example.org#p"),IntegerLiteral(4)),
		   RDFTriple(BNodeId(0),IRI("http://example.org#p"),LangLiteral("pepe",Lang("es")))
		   )
   val m = RDFTriples2Model(ts)
   m.write(System.out)
  }
 }

 }