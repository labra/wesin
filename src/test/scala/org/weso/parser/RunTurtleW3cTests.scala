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

object RunTurtleW3cTests
		extends TurtleParser
		with JenaMapper {

  val conf : Config = ConfigFactory.load()
  
  val manifestFile 	= conf.getString("manifestFile")
  val testsDir 		= conf.getString("TurtleTestsDir")
  val rdf  			= "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val rdft 			= "http://www.w3.org/ns/rdftest#"
  val mf 			= "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#"
  val w3cTestsURL 	= "http://www.w3.org/2013/TurtleTests/"

  val model 		= ModelFactory.createDefaultModel
  val turtleEval 	= model.createProperty(rdft + "TestTurtleEval")
  val action 		= model.createProperty(mf 	+ "action")
  val result 		= model.createProperty(mf 	+ "result")
  val mfname 		= model.createProperty(mf 	+ "name")
  val rdftype 		= model.createProperty(rdf 	+ "type")

  def createReport : Report = {
	
    model.read(manifestFile, testsDir,"TURTLE")

    val turtlePositiveSyntaxRs = get_resources(model,rdft+"TestTurtlePositiveSyntax")
    val turtleNegativeSyntaxRs = get_resources(model,rdft+"TestTurtleNegativeSyntax")
    val turtleEvalRs = get_resources(model,rdft+"TestTurtleEval")
    val turtleNegativeEvalRs = get_resources(model,rdft+"TestTurtleNegativeEval")
      
    val positiveSyntax = reportPositiveSyntax(model,turtlePositiveSyntaxRs)
    val negativeSyntax = reportNegativeSyntax(model,turtleNegativeSyntaxRs)
    val turtleEval 	   = reportPositiveTurtleEval(model,turtleEvalRs)
    val negativeEval   = reportNegativeTurtleEval(model,turtleNegativeEvalRs)

    positiveSyntax concat negativeSyntax concat turtleEval concat negativeEval
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
   
   def reportPositiveSyntax(m:Model,rs:List[Resource]) : Report = 
     rs.foldLeft(Report.initial)(positiveSyntax(m))

   def reportNegativeSyntax(m:Model,rs:List[Resource]) : Report = 
     rs.foldLeft(Report.initial)(negativeSyntax(m))

   def positiveSyntax(m:Model)(currentReport : Report,r: Resource): Report =  {     
       val action = getAction(m,r)
       val name   = getName(m,r)
       val baseIRI = w3cTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       val testType="Positive syntax"
       action match {
         case Some(node) if node.isURIResource() => {
        	 val contents = fromURL(node.asResource().getURI()).mkString 
        	 try { 
        	   TurtleParser.parse(contents,IRI(baseIRI)) match {
        		 	case Left(_) => 
        		 	  currentReport.addTestReport(true, name, testType, "Parsed OK")
        		 	case Right(msg) => 
        		 	  currentReport.addTestReport(false,name,testType, "Parsing failed with message: " + msg)
        		 }
        	 } catch {
        	  case e:Throwable => 
       		      currentReport.addTestReport(false,name,testType,"Exception raised when parsing: " + e)
        	 }
          }
          case _ => 
            currentReport.addTestReport(false, name, testType, "Cannot retrieve action for resource " + r)
       }
   }

   def negativeSyntax(m:Model)(currentReport : Report,r: Resource): Report =  {     
       val action = getAction(m,r)
       val name   = getName(m,r)
       val baseIRI = w3cTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       val testType="Negative syntax"
       action match {
         case Some(node) if node.isURIResource() => {
        	 val contents = fromURL(node.asResource().getURI()).mkString 
        	 try { 
        	   TurtleParser.parse(contents,IRI(baseIRI)) match {
        		 	case Left(_) => 
        		 	  currentReport.addTestReport(false, name, testType, "Parsed OK when expected to fail parsing")
        		 	case Right(msg) => 
        		 	  currentReport.addTestReport(true,name,testType, "Parsing failed with message: " + msg)
        		 }
        	 } catch {
        	  case e:Throwable => 
       		      currentReport.addTestReport(false,name,testType,"Exception raised when parsing: " + e)
        	 }
          }
          case _ => 
            currentReport.addTestReport(false, name, testType, "Cannot retrieve action for resource " + r)
       }
   }
  
   def reportPositiveTurtleEval(m:Model,rs:List[Resource]) : Report =
     rs.foldLeft(Report.initial)(positiveTurtleEval(m))
     
   def positiveTurtleEval(m:Model)(currentReport : Report,r: Resource): Report = {
       val action = getAction(m,r)
       val result = getResult(m,r)
       val name   = getName(m,r)
       val testType = "PositiveTurtleEval"
       val baseIRI = w3cTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       (action,result) match {
         case (Some(a),Some(r)) 
         if a.isURIResource && r.isURIResource => {
        	 val strAction = fromURL(a.asResource().getURI(),"UTF-8").mkString 
        	 val strResult = fromURL(r.asResource().getURI(),"UTF-8").mkString 
        	 val resultJenaParser = str2model(strResult,baseIRI,"N-TRIPLES")

        	 passTurtleEval(name,IRI(baseIRI),strAction,resultJenaParser) match {
        	   case Left(_) => currentReport.addTestReport(true,name,testType,"Models are isomorphic")
        	   case Right(msg) => currentReport.addTestReport(false, name, testType, msg)
        	 }
         }
         case x => 
           currentReport.addTestReport(false, name, testType, "Cannot retrieve (action,result) for resource " + r + ". Obtained: " + x)
       }
   }

   def reportNegativeTurtleEval(m:Model,rs:List[Resource]) : Report =
     rs.foldLeft(Report.initial)(negativeTurtleEval(m))
     
   def negativeTurtleEval(m:Model)(currentReport : Report,r: Resource): Report = {
       val action = getAction(m,r)
       val name   = getName(m,r)
       val testType = "Negative Turtle Eval"
       val baseIRI = w3cTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       action match {
         case Some(a) 
         if a.isURIResource => {
        	 val strAction = fromURL(a.asResource().getURI(),"UTF-8").mkString 

         	 try { 
        	   TurtleParser.parse(strAction,IRI(baseIRI)) match {
        		 	case Left(triples) => 
        		 	  val model = RDFTriples2Model(triples)
        		 	  currentReport.addTestReport(false, name, testType, "Parsed and model built when expected to fail parsing")
        		 	case Right(msg) => 
        		 	  currentReport.addTestReport(true,name,testType, "Parsing failed with message: " + msg)
        		 }
        	 } catch {
        	  case e:Throwable => 
       		      currentReport.addTestReport(true,name,testType,"Exception raised when parsing: " + e)
        	 }
         }
         case x => 
           currentReport.addTestReport(false, name, testType, "Cannot retrieve action for resource " + r)
       }
   }
   /*
   def passNegativeTurtleEval(m:Model,rs:List[Resource]) : Unit = {
     for ((r,n) <- rs zip (1 to rs.length)) {
       try {
       val action = getAction(m,r)
       val name   = getName(m,r)
       val baseIRI = w3cTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       action match {
         case Some(node) if node.isURIResource() => {
        	 val contents = fromURL(node.asResource().getURI()).mkString ;
        	 shouldNotPassTurtleEval("(" + n + ") " + name,IRI(baseIRI),contents)
         }
         case x => info("Cannot retrieve action for resource " + r + ". Obtained: " + x)	 
       }
      } catch {
       case e:Throwable => info("TurtleEval. Exception " + e + " raised for resource " + r)
       }
     }
   }
*/
   
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
     val iter = m.listObjectsOfProperty(r,mfname)
     if (iter.hasNext) {
       val node : RDFNode = iter.next()
       if (node.isLiteral) node.asLiteral().getLexicalForm()
       else "<resource " + r + " with no name>"
     } else "<resource " + r + " with no name>"
   }

  def passTurtleEval( name:String,
		   baseIRI: IRI,
		   in : String, 
		   expected: Model ) : Either[Model,String]= {
   try {
     TurtleParser.parse(in,baseIRI) match {
     case Left(triples) => {
          val model = RDFTriples2Model(triples)
          if (model.isIsomorphicWith(expected)) 
        	  Left(model)
          else 
        	  Right("Models are not isomorphic. ")
     }
     case Right(msg) => 
          	  Right("Test: " + name + ". Cannot parse: " + msg + "\n" + 
          	        in + "\n-----------------\n")
     }
   } catch {
     case e:Throwable => 
       		  Right("Exception: " + e + " raised in test: " + name)
   }
   
  }

 /*
   
 def shouldBeIsomorphicNamed(name:String,m1: Model, m2:Model) : Unit = {
   it("Should be isomorphic: " + name) {
    val b = m1.isIsomorphicWith(m2)
    if (!b) {
     info("Models to compare in test: " + name)
     info("-------------- Model 1:" + m1.toString)
     info("-------------- Model 2:" + m2.toString)
     info("-------------- Isomorphism m1 m2:" + m1.isIsomorphicWith(m2))
     infoModel("Model 1", m1)
     infoModel("Model 2", m2)
     fail("Models are not isomorphic: " + name) 
    }
  }
 } */

/*

 def shouldNotPassTurtleEval[S]
		 ( name:String, 
		   baseIRI: IRI,
		   in : String ) : Unit = {
  it("Should not pass turtle eval: " + name) {
   try {
     info("...trying " + name)
    val result = TurtleParser.parse(in,baseIRI) match {
    case Left(triples) => {
          info("Parser succeeded with triples " + triples)
//          val model = RDFTriples2Model(triples)
//          fail("Model parsed: " + model)
    }
    case Right(msg) => 
          	info("Test: " + name + " could not parse: " + msg)
    }
   } catch {
     case e:Throwable => info("Exception: " + e + " raised in test: " + name)
   }
  }
 }
*/
 /**
  * Convert a String to a Model
  * @param s String
  * @param base Base URL (default = empty String)
  * @param lang Syntax language. Can be: RDF/XML, N-TRIPLES, TURTLE. Default value: TURTLE
  */
 def str2model(s: String, 
		 base:String = "", 
		 lang:String = "TURTLE") : Model = {
   val m = ModelFactory.createDefaultModel
   val in : InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))
   m.read(in,base,lang)
   m
 }

 /*
 /**
  * TODO: This method could be removed or refactored
  */
 def passTurtleEvalSingle(m:Model, name:String) : Unit = {
     getResourceWithName(name,m) match {
       case Some(r) => {
         val action = getAction(m,r)
         val result = getResult(m,r)
         val baseIRI = w3cTestsURL + name + ".ttl"
         (action,result) match {
         	case (Some(a),Some(r)) 
         	if a.isURIResource && r.isURIResource => {
        	 val strAction = 
        	   fromURL(a.asResource().getURI(),"UTF-8").mkString ;
        	 val m1JenaParser = 
        			 str2model(strAction,
        					   w3cTestsURL + name + ".ttl", // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
        					   "TURTLE")

        	 val strResult = 
        	   fromURL(r.asResource().getURI(),"UTF-8").mkString ;
        	 val resultJenaParser = str2model(strResult,w3cTestsURL,"N-TRIPLES")
        	 
        	 // The following tests check that models read with Jena are isomorphic
        	 shouldBeIsomorphicNamed("Jena Models: " + name + ". Action: " + a + ". Result: " + r, m1JenaParser, resultJenaParser)
        	 
        	 shouldPassTurtleEval(name,IRI(baseIRI),strAction,resultJenaParser)
          }
         case x => println("Cannot retrieve (action,result) for resource " + r + ". Obtained: " + x)	 
       }
      }
      case None => println("No resource found with action " + name)
     }
   }

 def getResourceWithName(name: String, m: Model) : Option[Resource] = {
     val iter = m.listSubjectsWithProperty(mfname,name)
     if (iter.hasNext) {
       val node : Resource = iter.next()
       Some(node)
     } else None
   } */
}

