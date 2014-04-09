package es.weso.parser


import com.hp.hpl.jena.rdf.model.RDFNode

import es.weso.rdfgraph.nodes._


import com.typesafe.config._
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.rdf.model.Resource

import java.io.ByteArrayInputStream
import java.io.InputStream


import scala.io.Source._

import es.weso.jena.JenaMapper

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
         case Some(node) if node.isURIResource => {
        	 val contents = fromURL(node.asResource().getURI).mkString
        	 try { 
        	   TurtleParser.parse(contents,IRI(baseIRI)) match {
        		 	case util.Success(_) => 
        		 	  currentReport.addTestReport(true, name, r.getLocalName, testType, "Parsed OK")
        		 	case util.Failure(msg) => 
        		 	  currentReport.addTestReport(false,name, r.getLocalName, testType, "Parsing failed with message: " + msg)
        		 }
        	 } catch {
        	  case e:Throwable => 
       		      currentReport.addTestReport(false,name, r.getLocalName, testType,"Exception raised when parsing: " + e)
        	 }
          }
          case _ => 
            currentReport.addTestReport(false, name, r.getLocalName, testType, "Cannot retrieve action for resource " + r)
       }
   }

   def negativeSyntax(m:Model)(currentReport : Report,r: Resource): Report =  {     
       val action = getAction(m,r)
       val name   = getName(m,r)
       val baseIRI = w3cTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       val testType="Negative syntax"
       action match {
         case Some(node) if node.isURIResource => {
        	 val contents = fromURL(node.asResource().getURI()).mkString 
        	 try { 
        	   TurtleParser.parse(contents,IRI(baseIRI)) match {
        		 	case util.Success(_) => 
        		 	  currentReport.addTestReport(false, name, r.getLocalName, testType, "Parsed OK when expected to fail parsing")
        		 	case util.Failure(msg) => 
        		 	  currentReport.addTestReport(true,name, r.getLocalName, testType, "Parsing failed with message: " + msg)
        		 }
        	 } catch {
        	  case e:Throwable => 
       		      currentReport.addTestReport(false,name, r.getLocalName, testType,"Exception raised when parsing: " + e)
        	 }
          }
          case _ => 
            currentReport.addTestReport(false, name, r.getLocalName, testType, "Cannot retrieve action for resource " + r)
       }
   }
  
   def reportPositiveTurtleEval(m:Model,rs:List[Resource]) : Report =
     rs.foldLeft(Report.initial)(positiveTurtleEval(m))
     
   def positiveTurtleEval(m:Model)(currentReport : Report,res: Resource): Report = {
       val action = getAction(m,res)
       val result = getResult(m,res)
       val name   = getName(m,res)
       val testType = "PositiveTurtleEval"
       val baseIRI = w3cTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       (action,result) match {
         case (Some(a),Some(r)) 
         if a.isURIResource && r.isURIResource => {
        	 val strAction = fromURL(a.asResource().getURI,"UTF-8").mkString
        	 val strResult = fromURL(r.asResource().getURI,"UTF-8").mkString
        	 val resultJenaParser = str2model(strResult,baseIRI,"N-TRIPLES")

        	 passTurtleEval(name,IRI(baseIRI),strAction,resultJenaParser) match {
        	   case Left(_) => currentReport.addTestReport(true,name,res.getLocalName, testType,"Models are isomorphic")
        	   case Right(msg) => currentReport.addTestReport(false, name, res.getLocalName, testType, msg)
        	 }
         }
         case x => 
           currentReport.addTestReport(false, name, res.getLocalName, testType, "Cannot retrieve (action,result) for resource " + res + ". Obtained: " + x)
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
        	 val strAction = fromURL(a.asResource().getURI,"UTF-8").mkString

         	 try { 
        	   TurtleParser.parse(strAction,IRI(baseIRI)) match {
        		 	case util.Success((triples,pm)) => 
        		 	  val model = RDFTriples2Model(triples)
        		 	  currentReport.addTestReport(false, name, r.getLocalName, testType, "Parsed and model built when expected to fail parsing")
        		 	case util.Failure(msg) => 
        		 	  currentReport.addTestReport(true,name,r.getLocalName, testType, "Parsing failed with message: " + msg)
        		 }
        	 } catch {
        	  case e:Throwable => 
       		      currentReport.addTestReport(true,name,r.getLocalName, testType,"Exception raised when parsing: " + e)
        	 }
         }
         case x => 
           currentReport.addTestReport(false, name, r.getLocalName, testType, "Cannot retrieve action for resource " + r)
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
       if (node.isLiteral) node.asLiteral().getLexicalForm
       else "<resource " + r + " with no name>"
     } else "<resource " + r + " with no name>"
   }

  def passTurtleEval( name:String,
		   baseIRI: IRI,
		   in : String, 
		   expected: Model ) : Either[Model,String]= {
   try {
     TurtleParser.parse(in,baseIRI) match {
     case util.Success((triples,pm)) => {
          val model = RDFTriples2Model(triples)
          if (model.isIsomorphicWith(expected)) 
        	  Left(model)
          else 
        	  Right("Models are not isomorphic. \nModel    = " + model + "\nExpected = " + expected)
     }
     case util.Failure(msg) => 
          	  Right("Test: " + name + ". Cannot parse: " + msg + "\n" + 
          	        in + "\n-----------------\n")
     }
   } catch {
     case e:Throwable => 
       		  Right("Exception: " + e + " raised in test: " + name)
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

 def getResourceWithName(name: String, m: Model) : Option[Resource] = {
     val iter = m.listSubjectsWithProperty(mfname,name)
     if (iter.hasNext) {
       val node : Resource = iter.next()
       Some(node)
     } else None
   } 
}

