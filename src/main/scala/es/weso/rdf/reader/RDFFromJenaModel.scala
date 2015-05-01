package es.weso.rdf.reader

import com.hp.hpl.jena.query._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.nodes.RDFNode
import es.weso.rdfgraph.statements.RDFTriple
import scala.collection.JavaConversions._
import scala.collection.immutable.StringOps._
import scala.util.Try
import es.weso.rdfgraph.statements._
import com.hp.hpl.jena.rdf.model.{ RDFNode => JenaRDFNode }
import org.slf4j._
import org.apache.jena.riot.{ Lang => JenaLang }
import org.apache.jena.riot.RDFDataMgr
import com.hp.hpl.jena.rdf.model.ModelFactory
import es.weso.rdf._
import java.io.StringWriter
import com.hp.hpl.jena.rdf.model._
import scala.util._
import java.io._
import es.weso.rdf.reader.SPARQLQueries._

case class RDFFromJenaModel(model: Model) extends RDF {

  val log = LoggerFactory.getLogger("RDFFromJenaModel")

  override def parse(cs: CharSequence): Try[RDF] = {
    try {
      val m = ModelFactory.createDefaultModel
      RDFDataMgr.read(m, cs.toString)
      Success(RDFFromJenaModel(m))
    } catch {
      case e: Exception => Failure(throw new Exception("Exception parsing char sequence: " + e.getMessage))
    }
  }

  override def serialize(format: String): String = {
    val out: StringWriter = new StringWriter()
    model.write(out, format)
    out.toString
  }

  // TODO: this implementation only returns subjects
  override def iris(): Set[IRI] = {
    val resources: Set[Resource] = model.listSubjects().toSet().toSet
    resources.filter(s => s.isURIResource).map(r => IRI(r.getURI))
  }

  override def subjects(): Set[IRI] = {
    val resources: Set[Resource] = model.listSubjects().toSet().toSet
    resources.filter(s => s.isURIResource).map(r => IRI(r.getURI))
  }

  override def rdfTriples(): Set[RDFTriple] = {
    throw new Exception("Cannot obtain triples from RDFFromWeb ")
  }

  override def triplesWithSubject(node: RDFNode): Set[RDFTriple] = {
    if (node.isIRI) {
      val subj = node.toIRI
      val m = QueryExecutionFactory.create(queryTriplesWithSubject(subj), model).execConstruct()
      val triples = model2triples(m)
      log.debug("triples with subject " + subj + " =\n" + triples)
      triples
    } else
      throw new Exception("triplesWithSubject: node " + node + " must be a IRI")
  }

  override def triplesWithObject(node: RDFNode): Set[RDFTriple] = {
    if (node.isIRI) {
      val obj = node.toIRI
      val m = QueryExecutionFactory.create(queryTriplesWithObject(obj), model).execConstruct()
      model2triples(m)
    } else
      throw new Exception("triplesWithObject: node " + node + " must be a IRI")
  }

  override def triplesWithPredicateObject(p: IRI, node: RDFNode): Set[RDFTriple] = {
    if (node.isIRI) {
      val obj = node.toIRI
      val derefModel = ModelFactory.createDefaultModel
      RDFDataMgr.read(derefModel, obj.str)
      val model = QueryExecutionFactory.create(queryTriplesWithPredicateObject(p, obj), derefModel).execConstruct()
      model2triples(model)
    } else
      throw new Exception("triplesWithObject: node " + node + " must be a IRI")
  }

  def model2triples(model: Model): Set[RDFTriple] = {
    model.listStatements().map(st => statement2triple(st)).toSet
  }

  def statement2triple(st: Statement): RDFTriple = {
    RDFTriple(
      jena2rdfnode(st.getSubject),
      property2iri(st.getPredicate),
      jena2rdfnode(st.getObject))
  }

  def property2iri(p: Property): IRI = {
    IRI(p.getURI)
  }

  def jena2rdfnode(r: JenaRDFNode): RDFNode = {
    if (r.isAnon) {
      BNodeId(r.asNode.getBlankNodeId().hashCode())
    } else if (r.isURIResource) {
      IRI(r.asResource.getURI())
    } else if (r.isLiteral) {
      val lit = r.asLiteral
      if (lit.getDatatypeURI() == null) {
        StringLiteral(lit.getString())
      } else
        IRI(lit.getDatatypeURI()) match {
          case RDFNode.IntegerDatatypeIRI => IntegerLiteral(lit.getInt)
          case RDFNode.BooleanDatatypeIRI => BooleanLiteral(lit.getBoolean)
          case RDFNode.DoubleDatatypeIRI => DoubleLiteral(lit.getDouble())
          case RDFNode.LangStringDatatypeIRI => LangLiteral(lit.getLexicalForm, Lang(lit.getLanguage))
          case _ => DatatypeLiteral(lit.getLexicalForm, IRI(lit.getDatatypeURI))
        }
    } else
      throw new Exception("Unknown type of resource")
  }

  override def prefixMap: PrefixMap = {
    PrefixMap(model.getNsPrefixMap.toMap.map(pair => (pair._1, IRI(pair._2))))
  }
}

object RDFFromJenaModel {

  def fromURI(uri: String): Try[RDF] = {
    try {
      val m = ModelFactory.createDefaultModel()
      RDFDataMgr.read(m, uri)
      Success(RDFFromJenaModel(m))
    } catch {
      case e: Exception => Failure(throw new Exception("Exception accessing  " + uri + ": " + e.getMessage))
    }
  }

  def fromFile(file: File): Try[RDF] = {
    try {
      val m = ModelFactory.createDefaultModel()
      val is: InputStream = new FileInputStream(file)
      RDFDataMgr.read(m, is, JenaLang.TURTLE)
      Success(RDFFromJenaModel(m))
    } catch {
      case e: Exception => Failure(throw new Exception("Exception accessing  " + file.getName + ": " + e.getMessage))
    }
  }

}