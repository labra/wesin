package es.weso.jena

import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.rdf.model.{RDFNode => JenaRDFNode}
import com.hp.hpl.jena.rdf.model.Property
import com.hp.hpl.jena.rdf.model.Resource
import es.weso.rdfgraph.nodes._
import com.hp.hpl.jena.rdf.model.AnonId
import com.hp.hpl.jena.datatypes.BaseDatatype
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import com.hp.hpl.jena.rdf.model.Model
import es.weso.rdfgraph.statements.RDFTriple

trait JenaMapper {

  def RDFTriples2Model(triples: Set[RDFTriple]) : JenaModel = {
    val m = ModelFactory.createDefaultModel()
    for (t <- triples) {
     val subj = createResource(m,t.subj)
     val pred = createProperty(m,t.pred)
     val obj  = createRDFNode(m,t.obj)
     val stmt = m.createStatement(subj, pred, obj)
     m.add(stmt)
    }
    m
  }

  def createResource(m:JenaModel,node:RDFNode) : Resource = {
    node match {
      case BNodeId(id) => m.createResource(new AnonId(id.toString))
      case i : IRI => m.createResource(i.str)
      case _ => throw new Exception("Cannot create a resource from " + node)
    }
  }

  def createRDFNode(m:JenaModel,node:RDFNode) : JenaRDFNode = {
    val xsd = "http://www.w3.org/2001/XMLSchema#"
    val xsdinteger= xsd + "integer"
    val xsddouble = xsd + "double"
    val xsddecimal = xsd + "decimal"
    val xsdboolean = xsd + "boolean"

    node match {
     case BNodeId(id) 						 => 
       	m.createResource(new AnonId(id.toString))
     case i: IRI 							 => 
       	m.createResource(i.str)
     case StringLiteral(str) 				 => 
       	m.createLiteral(str,false)
     case DatatypeLiteral(str,i:IRI) => {
        i.str match {
          case `xsdinteger` => m.createTypedLiteral(str,XSDDatatype.XSDinteger) 
          case `xsddouble` => m.createTypedLiteral(str,XSDDatatype.XSDdouble)
          case `xsddecimal` => m.createTypedLiteral(str,XSDDatatype.XSDdecimal)
          case `xsdboolean` => m.createTypedLiteral(str,XSDDatatype.XSDboolean)
          case _ => m.createTypedLiteral(str,new BaseDatatype(i.str))
        }
     }
     case DecimalLiteral(d) 		=> 
     	m.createTypedLiteral(d.toString,XSDDatatype.XSDdecimal)
     case IntegerLiteral(i) 		=> 
       	m.createTypedLiteral(i.toString,XSDDatatype.XSDinteger)
     case LangLiteral(l,Lang(lang)) => m.createLiteral(l,lang)
     case BooleanLiteral(b) => 
        m.createTypedLiteral(b.toString,XSDDatatype.XSDboolean)
     case DoubleLiteral(d: Double) => 
        m.createTypedLiteral(d.toString,XSDDatatype.XSDdouble)
     case _ => 
       throw new Exception("Cannot create a resource from " + node)
   }  
  }

  def createProperty(m:Model,pred:IRI) : Property = {
    m.createProperty(pred.str)
  }
  
}