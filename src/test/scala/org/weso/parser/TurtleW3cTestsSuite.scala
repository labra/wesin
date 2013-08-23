package org.weso.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scala.io.Source
// import org.weso.rdfNode._
import com.hp.hpl.jena.rdf.model.RDFNode
import org.weso.rdfTriple._
import scala.util.parsing.input.CharArrayReader
import com.typesafe.config._
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.vocabulary.RDF
import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.rdf.model.Resource



class TurtleW3cTestsSuite 
		extends TurtleParser 
		with FunSpec 
		with ShouldMatchers
		with TestParser {
  
  val conf : Config = ConfigFactory.load()
  implicit val s : TurtleParserState = TurtleParserState.initial
  
  val manifestFile = conf.getString("manifestFile")
  val testsDir 	= conf.getString("TurtleTestsDir")
  val rdf  		= "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val rdft 		= "http://www.w3.org/ns/rdftest#"
  val mf 		= "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#"
       
  val model = ModelFactory.createDefaultModel

  val rdftype 		= model.createProperty(rdf 	+ "type")
  val turtleEval 	= model.createProperty(rdft + "TestTurtleEval")
  val action 		= model.createProperty(mf 	+ "action")
  val name 			= model.createProperty(mf 	+ "name")
  

  describe("Turtle Parser using W3c tests") {

    describe("parse manifest file") {

       
	   val f = "file:///C:/src/wesin/TurtleTests/turtle-syntax-number-09.ttl"
       val cs = scala.io.Source.fromURL(f).mkString
       println(cs)
	   
       model.read(manifestFile, testsDir,"TURTLE")

       val turtlePositiveSyntaxRs = get_resources(model,rdft+"TestTurtlePositiveSyntax")
       show_resources(model,"Turtle Positive Syntax",turtlePositiveSyntaxRs)
       passPositiveSyntax(model,turtlePositiveSyntaxRs)
       
       val turtleNegativeSyntaxRs = get_resources(model,rdft+"TestTurtleNegativeSyntax")
       show_resources(model,"Turtle Negative Syntax",turtleNegativeSyntaxRs)
       
       val turtleEvalRs = get_resources(model,rdft+"TestTurtleEval")
       show_resources(model,"Turtle Eval",turtleEvalRs)

       val turtleNegativeEvalRs = get_resources(model,rdft+"TestTurtleNegativeEval")
       show_resources(model,"Turtle Negative Eval",turtleNegativeEvalRs)

       val ntriplesPositiveSyntaxRs = get_resources(model,rdft+"TestNTriplesPositiveSyntax")
       show_resources(model,"NTriples Positive Syntax",ntriplesPositiveSyntaxRs)

       val ntriplesNegativeSyntaxRs = get_resources(model,rdft+"TestNTriplesNegativeSyntax ")
       show_resources(model,"NTriples Negative Syntax",ntriplesNegativeSyntaxRs)
       
}

   def show_resources(m: Model, msg: String, rs: List[Resource]) : Unit = {
     println(msg)
     println("No. of resources = " + rs.length)
   }
   
   def get_resources(m : Model,t:String): List[Resource] = {
     var resultSet = scala.collection.mutable.Set[Resource]()
     val resType = m.createProperty(t)
     val iter = m.listSubjectsWithProperty(rdftype,resType)
       while (iter.hasNext) {
         resultSet += iter.next
       }
     resultSet.toList
   }
   
   def passPositiveSyntax(m:Model,rs:List[Resource]) : Unit = {
     
     for (r <- rs) {
       val action = getAction(m,r)
       val name   = getName(m,r)
       action match {
         case Some(node) if node.isURIResource() => {
        	 val contents = scala.io.Source.fromURL(node.asResource().getURI()).mkString ;
        	 shouldParseNamed(name,turtleDoc(s),contents)
         }
         case _ => println("Cannot retrieve action for resource " + r)	 
       }
     }
   }

   
   def getAction(m: Model, r:Resource) : Option[RDFNode] = {
     val iter = m.listObjectsOfProperty(r,action)
     if (iter.hasNext) {
       val node : RDFNode = iter.next()
       Some(node)
     } else None
   }
   
   def getName(m: Model, r:Resource) : String = {
     val iter = m.listObjectsOfProperty(r,name)
     if (iter.hasNext) {
       val node : RDFNode = iter.next()
       if (node.isLiteral) node.asLiteral().getLexicalForm()
       else "<resource " + r + " with no name>"
     } else "<resource " + r + " with no name>"
   }
   
   /*   describe("parse test ntriple file") {
     implicit val s = ParserState.initial
     val p = parser.turtleDoc

     val input = Source.fromURL(getClass.getResource("/test.nt")).mkString
     shouldParseGen(p,input)
   } */
   
  }
}