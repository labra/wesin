package es.weso.rdf.jena

// TODO: Refactor this code 
import com.hp.hpl.jena.rdf.model.{
  Model => JenaModel,
  Statement,
  ModelFactory,
  RDFNode => JenaRDFNode,
  Property,
  Resource,
  AnonId
}
import es.weso.rdfgraph.nodes._
import com.hp.hpl.jena.datatypes.BaseDatatype
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import es.weso.rdfgraph.statements.RDFTriple

object JenaMapper {

  def RDFTriples2Model(triples: Set[RDFTriple], m: JenaModel): JenaModel = {
    for (t <- triples) {
      val subj = createResource(m, t.subj)
      val pred = createProperty(m, t.pred)
      val obj = createRDFNode(m, t.obj)
      val stmt = m.createStatement(subj, pred, obj)
      m.add(stmt)
    }
    m
  }

  /*  def RDFTriples2Model(triples: Set[RDFTriple]): JenaModel = {
    val m = ModelFactory.createDefaultModel()
    for (t <- triples) {
      val subj = createResource(m, t.subj)
      val pred = createProperty(m, t.pred)
      val obj = createRDFNode(m, t.obj)
      val stmt = m.createStatement(subj, pred, obj)
      m.add(stmt)
    }
    m
  } */

  def RDFTriple2Statement(triple: RDFTriple): Statement = {
    ???
  }

  def createResource(m: JenaModel, node: RDFNode): Resource = {
    node match {
      case BNodeId(id) => m.createResource(new AnonId(id.toString))
      case i: IRI => m.createResource(i.str)
      case _ => throw new Exception("Cannot create a resource from " + node)
    }
  }

  def createRDFNode(m: JenaModel, node: RDFNode): JenaRDFNode = {
    val xsd = "http://www.w3.org/2001/XMLSchema#"
    val xsdinteger = xsd + "integer"
    val xsddouble = xsd + "double"
    val xsddecimal = xsd + "decimal"
    val xsdboolean = xsd + "boolean"

    node match {
      case BNodeId(id) =>
        m.createResource(new AnonId(id.toString))
      case i: IRI =>
        m.createResource(i.str)
      case StringLiteral(str) =>
        m.createLiteral(str, false)
      case DatatypeLiteral(str, i: IRI) =>
        i.str match {
          case `xsdinteger` => m.createTypedLiteral(str, XSDDatatype.XSDinteger)
          case `xsddouble` => m.createTypedLiteral(str, XSDDatatype.XSDdouble)
          case `xsddecimal` => m.createTypedLiteral(str, XSDDatatype.XSDdecimal)
          case `xsdboolean` => m.createTypedLiteral(str, XSDDatatype.XSDboolean)
          case _ => m.createTypedLiteral(str, new BaseDatatype(i.str))
        }
      case DecimalLiteral(d) =>
        m.createTypedLiteral(d.toString(), XSDDatatype.XSDdecimal)
      case IntegerLiteral(i) =>
        m.createTypedLiteral(i.toString, XSDDatatype.XSDinteger)
      case LangLiteral(l, Lang(lang)) => m.createLiteral(l, lang)
      case BooleanLiteral(b) =>
        m.createTypedLiteral(b.toString, XSDDatatype.XSDboolean)
      case DoubleLiteral(d: Double) =>
        m.createTypedLiteral(d.toString, XSDDatatype.XSDdouble)
      case _ =>
        throw new Exception("Cannot create a resource from " + node)
    }
  }

  def createProperty(m: JenaModel, pred: IRI): Property = {
    m.createProperty(pred.str)
  }

}