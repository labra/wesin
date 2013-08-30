package org.weso.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scala.io.Source
import com.hp.hpl.jena.rdf.model.RDFNode
import org.weso.rdfTriple._
import scala.util.parsing.input.CharArrayReader
import com.typesafe.config._
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.vocabulary.RDF
import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.rdf.model.Resource
import java.io.ByteArrayInputStream
import java.io.InputStream
import org.weso.rdfTriple.jenaMapper.JenaMapper
import scala.io.Source._
import com.hp.hpl.jena.rdf.model.Literal
import org.weso.rdfNode.IRI
import java.io.FileOutputStream

class RunTurtleW3cTestsSuite extends FunSpec {
 val report = RunTurtleW3cTests.createReport
 
 describe("W3c tests report") {
   for ((r,n) <- report.items zip (1 to report.items.length))
   it("Should pass test " + n + ": " + r.name) {
     if (r.passed) info("Info: " + r)
     else fail("Test did not pass" + r)
   } 
 }
 
  describe("Generate W3c EARL report") {
    it("Should Generate EARL report") {
      val earlModel = report.generateEARL
      val conf : Config = ConfigFactory.load()
      val outFile = conf.getString("EarlReportFile")

      earlModel.write(System.out,"TURTLE")
      earlModel.write(new FileOutputStream(outFile),"TURTLE")
   }
 }

}