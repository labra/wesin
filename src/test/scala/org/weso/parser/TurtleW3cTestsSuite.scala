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
  val w3cTestsURL = "http://www.w3.org/2013/TurtleTests/"
    
  val model = ModelFactory.createDefaultModel

  val rdftype 		= model.createProperty(rdf 	+ "type")
  val turtleEval 	= model.createProperty(rdft + "TestTurtleEval")
  val action 		= model.createProperty(mf 	+ "action")
  val result 		= model.createProperty(mf 	+ "result")
  val mfname 		= model.createProperty(mf 	+ "name")
  

  describe("Turtle Parser using W3c tests") {

    describe("parse manifest file") {

      model.read(manifestFile, testsDir,"TURTLE")

      val turtlePositiveSyntaxRs = get_resources(model,rdft+"TestTurtlePositiveSyntax")
      val turtleNegativeSyntaxRs = get_resources(model,rdft+"TestTurtleNegativeSyntax")
      val turtleEvalRs = get_resources(model,rdft+"TestTurtleEval")
      val turtleNegativeEvalRs = get_resources(model,rdft+"TestTurtleNegativeEval")
      
      passPositiveSyntax(model,turtlePositiveSyntaxRs)
      passNegativeSyntax(model,turtleNegativeSyntaxRs)
      passTurtleEval(model,turtleEvalRs)
      passNegativeTurtleEval(model,turtleNegativeEvalRs)
      
      // passTurtleEvalSingle(model,"IRIREF_datatype")

     }
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
     for ((r,n) <- rs zip (1 to rs.length)) {
       val action = getAction(m,r)
       val name   = getName(m,r)
       action match {
         case Some(node) if node.isURIResource() => {
        	 val contents = fromURL(node.asResource().getURI()).mkString ;
        	 shouldParseNamed("(" + n + ") " + name,turtleDoc(s),contents)
         }
         case _ => println("Cannot retrieve action for resource " + r)	 
       }
     }
   }

   def passNegativeSyntax(m:Model,rs:List[Resource]) : Unit = {
     for ((r,n) <- rs zip (1 to rs.length)) {
       val action = getAction(m,r)
       val name   = getName(m,r)
       action match {
         case Some(node) if node.isURIResource() => {
        	 val contents = fromURL(node.asResource().getURI()).mkString ;
        	 shouldNotParseNamed("(" + n + ") " + name,turtleDoc(s),contents)
         }
         case _ => println("Cannot retrieve action for resource " + r)	 
       }
     }
   }

 
   def passTurtleEval(m:Model,rs:List[Resource]) : Unit = {
     for ((r,n) <- rs zip (1 to rs.length)) {
       try {
       val action = getAction(m,r)
       val result = getResult(m,r)
       val name   = getName(m,r)
       val baseIRI = w3cTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       (action,result) match {
         case (Some(a),Some(r)) 
         if a.isURIResource && r.isURIResource => {
        	 val strAction = fromURL(a.asResource().getURI(),"UTF-8").mkString 
        	 
        	 val strResult = fromURL(r.asResource().getURI(),"UTF-8").mkString 
        	 val resultJenaParser = str2model(strResult,baseIRI,"N-TRIPLES")
        	 
        	 shouldPassTurtleEval("(" + n + ") " + name,IRI(baseIRI),strAction,resultJenaParser)
         }
         case x => info("Cannot retrieve (action,result) for resource " + r + ". Obtained: " + x)	 
       }
     } catch {
       case e:Throwable => info("TurtleEval. Exception " + e + " raised for resource " + r)
       }
     }
   }

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
 }

 /*
  * Show more info of a model
  */
 def infoModel(name: String, m:Model) : Unit = {
   info("Model: " + name)
   val iter = m.listStatements
   while (iter.hasNext) {
     info("Stmt: " + iter.next.toString)
   }
 } 

 /**
  * Compare first statement in two models
  */
 def compareFirstStatements(m1:Model, m2:Model) : Unit = {
   println("Comparing statements...")
   val iter1 = m1.listStatements
   val iter2 = m2.listStatements
   val s1 = iter1.nextStatement
   val s2 = iter2.nextStatement
     info("---Statements equal?: " + s1.equals(s2))
     info("---Subjects equal?: " + s1.getSubject.equals(s2.getSubject))
     info("---Predicates equal?: " + s1.getPredicate.equals(s2.getPredicate))
     info("---Literals equal?: " + s1.getLiteral.equals(s2.getLiteral))
     info("---Literals sameValues?: " + s1.getLiteral.sameValueAs(s2.getLiteral))
     info("---Literal datatype equal?: " + (s1.getLiteral.getDatatypeURI == s2.getLiteral.getDatatype))
     compareLiterals(s1.getLiteral,s2.getLiteral)
 } 

 /**
  * A strong comparison between literals...
  */
 def compareLiterals(l1:Literal, l2:Literal) : Unit = {
   println("Comparing literals...")
   info("Literal 1: " + l1 +           ". Lexical form: " + l1.getLexicalForm() + ". Datatype: " + l1.getDatatype())
   info("Literal 2: " + l1 +           ". Lexical form: " + l2.getLexicalForm() + ". Datatype: " + l2.getDatatype())
   info("Comparison:" + l1.equals(l2) +". Lexical form: " + (l1.getLexicalForm == l2.getLexicalForm) + ". Datatype: " + (l1.getDatatype() == l2.getDatatype))
 }

 def shouldPassTurtleEval[S]
		 ( name:String,
		   baseIRI: IRI,
		   in : String, 
		   expected: Model ) : Unit = {
   try {
    val result = TurtleParser.parse(in,baseIRI) match {
    case Left(triples) => {
          val model = RDFTriples2Model(triples)
          shouldBeIsomorphicNamed(name,model, expected)
    }
    case Right(msg) => 
          	fail("Test: " + name + ". Cannot parse: " + msg + "\n" + 
          	     in + "\n-----------------\n")
    }
   } catch {
     case e:Throwable => fail("Exception: " + e + " raised in test: " + name)
   }
 }

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
   }
 

}