package es.weso.rdf.jena

// TODO: Refactor this code 
import com.hp.hpl.jena.rdf.model.{
  Model => JenaModel,
  Statement,
  StmtIterator,
  ModelFactory,
  RDFNode => JenaRDFNode,
  Property,
  Resource,
  Literal,
  AnonId
}
import es.weso.rdf.nodes._
import com.hp.hpl.jena.datatypes.BaseDatatype
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.PREFIXES._
import scala.collection.JavaConversions._

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

  def RDFTriple2Statement(triple: RDFTriple): Statement = {
    // TODO: implement
    throw new Exception("RDFTriple2Statement: unimplemented conversion from " + triple)
  }

  def statement2RDFTriple(s: Statement): RDFTriple = {
    val subj: RDFNode = jenaNode2RDFNode(s.getSubject)
    val pred: IRI = property2IRI(s.getPredicate)
    val obj: RDFNode = jenaNode2RDFNode(s.getObject)
    RDFTriple(subj, pred, obj)
  }

  def rdfNode2Property(n: RDFNode, m: JenaModel): Property = {
    n match {
      case i: IRI => m.getProperty(i.str)
      case _      => throw new Exception("rdfNode2Property: unexpected node " + n)
    }
  }

  def rdfNode2Resource(n: RDFNode, m: JenaModel): Option[Resource] = {
    n match {
      case i: IRI => Some(m.getResource(i.str))
      case BNodeId(id) => {
        // Creates the BNode if it doesn't exist
        Some(m.createResource(new AnonId(id)))
      }
      case _ => None
    }
  }

  def rdfNode2JenaNode(n: RDFNode, m: JenaModel): JenaRDFNode = {
    n match {
      case i: IRI => m.getResource(i.str)
      case BNodeId(id) => {
        // Creates the BNode if it doesn't exist
        m.createResource(new AnonId(id))
      }
      case IntegerLiteral(n)            => m.createTypedLiteral(n)
      case DecimalLiteral(d)            => m.createTypedLiteral(d)
      //      case BooleanLiteral(b) => m.createLiteral(b)
      case LangLiteral(str, Lang(lang)) => m.createLiteral(str, lang)
      case _                            => throw new Exception("rdfNode2JenaNode: unexpected node " + n)
    }
  }

  def jenaNode2RDFNode(r: JenaRDFNode): RDFNode = {
    if (r.isURIResource()) {
      IRI(r.asResource().getURI)
    } else if (r.isAnon) {
      BNodeId(r.asResource().getId.getLabelString)
    } else if (r.isLiteral) {
      val lit = r.asLiteral()
      if (lit.getLanguage() != "") {
        LangLiteral(lit.getLexicalForm, Lang(lit.getLanguage))
      } else {
        val maybeDatatype = lit.getDatatypeURI
        if (maybeDatatype == null) {
          StringLiteral(lit.getString())
        } else {
        val datatype = IRI(maybeDatatype)
        datatype match {
          case RDFNode.StringDatatypeIRI     => StringLiteral(lit.getLexicalForm)
          case RDFNode.IntegerDatatypeIRI    => IntegerLiteral(lit.getLexicalForm.toInt)
          case RDFNode.DecimalDatatypeIRI    => DecimalLiteral(lit.getLexicalForm.toDouble)
          case RDFNode.BooleanDatatypeIRI    => BooleanLiteral(lit.getLexicalForm.toBoolean)
          case RDFNode.LangStringDatatypeIRI => LangLiteral(lit.getLexicalForm, Lang(lit.getLanguage))
          case _                => DatatypeLiteral(lit.getLexicalForm, datatype)
        }
        }
      }
    } else throw new Exception("resource2RDFNode: unexpected type of resource")
  }

  def property2IRI(p: Property): IRI = IRI(p.getURI)

  def createResource(m: JenaModel, node: RDFNode): Resource = {
    node match {
      case BNodeId(id) => m.createResource(new AnonId(id.toString))
      case i: IRI      => m.createResource(i.str)
      case _           => throw new Exception("Cannot create a resource from " + node)
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
          case `xsddouble`  => m.createTypedLiteral(str, XSDDatatype.XSDdouble)
          case `xsddecimal` => m.createTypedLiteral(str, XSDDatatype.XSDdecimal)
          case `xsdboolean` => m.createTypedLiteral(str, XSDDatatype.XSDboolean)
          case _            => m.createTypedLiteral(str, new BaseDatatype(i.str))
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

  def triplesSubject(resource: Resource, model: JenaModel): Set[Statement] = {
    model.listStatements(resource, null, null).toSet.toSet
  }

  def triplesPredicate(pred: Property, model: JenaModel): Set[Statement] = {
    model.listStatements(null, pred, null).toSet.toSet
  }

  def triplesObject(obj: Resource, model: JenaModel): Set[Statement] = {
    model.listStatements(null, null, obj).toSet.toSet
  }

  def triplesPredicateObject(property: Property, obj: Resource, model: JenaModel): Set[Statement] = {
    model.listStatements(null, property, obj).toSet.toSet
  }

}