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

case class Report(val items: List[SingleTestReport]) {
  def addTestReport(r: SingleTestReport) : Report = {
    Report(r :: items)
  }
  
  def addTestReport(passed:Boolean,name:String,testType: String, msg:String) : Report = {
    Report(SingleTestReport(passed,name,testType,msg) :: items)
  }
  
  def concat(other: Report) : Report = {
    Report(items ++ other.items)
  }
  
  def generateEARL : Model = {
    val model = ModelFactory.createDefaultModel()
    model
  }
}

object Report {
  def initial = Report(List())
}

case class SingleTestReport(
		val passed: Boolean, 	// True if test passed
		val name: String,    	// Name of test
		val testType: String, 	// Type of test 
		val moreInfo: String 	// Info about what happened
		) {
  override def toString : String = 
    if (passed) testType + ". OK " + name + ", " + moreInfo 
    else testType + ". Failed " + name + ", " + moreInfo  
}

