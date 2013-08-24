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

class TurtleW3cTestsSuite 
		extends TurtleParser
		with JenaMapper
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
  val result 		= model.createProperty(mf 	+ "result")
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
       passTurtleEval(model,turtleEvalRs)

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

   def passTurtleEval(m:Model,rs:List[Resource]) : Unit = {
     for (r <- rs) {
       val action = getAction(m,r)
       val result = getResult(m,r)
       val name   = getName(m,r)
       (action,result) match {
         case (Some(a),Some(r)) 
         if a.isURIResource && r.isURIResource => {
        	 val strAction = scala.io.Source.fromURL(a.asResource().getURI(),"UTF-8").mkString ;
        	 val m1JenaParser = str2model(strAction)

        	 val strResult = scala.io.Source.fromURL(r.asResource().getURI(),"UTF-8").mkString ;
        	 val resultJenaParser = str2model(strResult,"N-TRIPLES")
        	 shouldBeIsomorphicNamed("Jena Models: " + name + ".\n Action: " + a + ".\n Result: " + r,
        			 				  m1JenaParser, resultJenaParser)
        	 shouldPassTurtleEval(name,turtleDoc(s),strAction,resultJenaParser)
         }
         case x => println("Cannot retrieve (action,result) for resource " + r + ". Obtained: " + x)	 
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
   
   def getResult(m: Model, r:Resource) : Option[RDFNode] = {
     val iter = m.listObjectsOfProperty(r,result)
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
   
  }
  
 def shouldBeIsomorphicNamed(name:String,m1: Model, m2:Model) : Unit = {
   it("Should be isomorphic: " + name) {
    val b = m1.isIsomorphicWith(m2)
    if (!b) {
     info("Models are not isomorphic: " + name)
     info("-------------- Model 1:" + m1.toString)
     info("-------------- Model 2:" + m2.toString)
     fail("Models are not isomorphic: " + name)
    }
  }
 }
  
 def shouldPassTurtleEval[S]
		 ( name:String, 
		   p : Parser[(List[RDFTriple],TurtleParserState)],
		   in : String, 
		   expected: Model ) : Unit = {
   it("Should pass turtle eval test: " + name) {
	   val result = parseAll(p,in) match {
        case Success((triples,_),_) => {
          val model = RDFTriples2Model(triples)
          shouldBeIsomorphicNamed(name,model, expected)
        }
        case NoSuccess(msg,_) => 
          	fail("Cannot parse: " + msg + "\n" + 
          	     in + "\n-----------------\n")
      }   
   }
 }

 def str2model(s: String,lang:String = "TURTLE") : Model = {
   val m = ModelFactory.createDefaultModel
   val in : InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))
   m.read(in,"",lang)
   m
  }

}