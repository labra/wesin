package es.weso.rdf.jena

import com.hp.hpl.jena.query._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.nodes.RDFNode
import es.weso.rdfgraph.statements.RDFTriple
import scala.collection.JavaConversions._
import scala.collection.immutable.StringOps._
import scala.util.Try
import es.weso.rdfgraph.statements._
import es.weso.rdf._
import com.hp.hpl.jena.rdf.model.{
  Model,
  Resource,
  Property,
  Statement,
  RDFNode => JenaRDFNode,
  RDFReader => JenaRDFReader
}
import org.slf4j._
import org.apache.jena.riot.{ Lang => JenaLang }
import org.apache.jena.riot.RDFDataMgr
import com.hp.hpl.jena.rdf.model.ModelFactory
import java.io._
import scala.util._
import java.io._
import es.weso.rdf.jena.SPARQLQueries._
import org.apache.jena.riot.RDFLanguages._

case class RDFAsJenaModel(model: Model)
    extends RDFReader
    with RDFBuilder {
  type Rdf = RDFAsJenaModel

  val log = LoggerFactory.getLogger("RDFFromJenaModel")

  override def parse(cs: CharSequence, format: String = "TURTLE"): Try[RDFAsJenaModel] = {
    try {
      val m = ModelFactory.createDefaultModel
      val str_reader = new StringReader(cs.toString)
      RDFDataMgr.read(m, str_reader, "", shortnameToLang(format))
      Success(RDFAsJenaModel(m))
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
    model2triples(model)
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

  override def getPrefixMap: PrefixMap = {
    PrefixMap(model.getNsPrefixMap.toMap.map { case (alias, iri) => (alias, IRI(iri)) })
  }

  override def addPrefixMap(pm: PrefixMap): RDFAsJenaModel = {
    val map: Map[String, String] = pm.pm.map { case (str, iri) => (str, iri.str) }
    model.setNsPrefixes(map)
    this
  }

  override def addTriples(triples: Set[RDFTriple]): RDFAsJenaModel = {
    val newModel = JenaMapper.RDFTriples2Model(triples, model)
    model.add(newModel)
    this
  }

  // TODO: This is not efficient
  override def rmTriple(triple: RDFTriple): RDFAsJenaModel = {
    val empty = ModelFactory.createDefaultModel
    val model2delete = JenaMapper.RDFTriples2Model(Set(triple), empty)
    model.difference(model2delete)
    this
  }

  // TODO: Check the safeness of using just the hash code to ensure uniqueness of blank nodes
  //       Potential problem: interaction between this code and function newBnodeId
  //       Possible solution return resource.getId.getLabelString 
  override def createBNode: (RDFNode, RDFAsJenaModel) = {
    val resource = model.createResource
    (BNodeId(resource.hashCode), this)
  }

  override def addPrefix(alias: String, iri: String): RDFAsJenaModel = {
    model.setNsPrefix(alias, iri)
    this
  }

  def qName(str: String): IRI = {
    IRI(model.expandPrefix(str))
  }
}

object RDFAsJenaModel {

  def empty: RDFAsJenaModel = {
    RDFAsJenaModel(ModelFactory.createDefaultModel);
  }

  def fromURI(uri: String): Try[RDFAsJenaModel] = {
    try {
      val m = ModelFactory.createDefaultModel()
      RDFDataMgr.read(m, uri)
      Success(RDFAsJenaModel(m))
    } catch {
      case e: Exception => Failure(throw new Exception("Exception accessing  " + uri + ": " + e.getMessage))
    }
  }

  def fromFile(file: File, format: String): Try[RDFAsJenaModel] = {
    try {
      val m = ModelFactory.createDefaultModel()
      val is: InputStream = new FileInputStream(file)
      RDFDataMgr.read(m, is, shortnameToLang(format))
      Success(RDFAsJenaModel(m))
    } catch {
      case e: Exception => Failure(throw new Exception("Exception accessing  " + file.getName + ": " + e.getMessage))
    }
  }

  def fromChars(cs: CharSequence, format: String): Try[RDFAsJenaModel] = {
    try {
      RDFAsJenaModel.empty.parse(cs, format)
    } catch {
      case e: Exception => Failure(throw new Exception("Exception reading  " + formatLines(cs.toString) + "\n " + e.getMessage))
    }
  }

  def formatLines(cs: CharSequence): String = {
    cs.toString.lines.zipWithIndex.map(p => (p._2 + 1).toString + " " + p._1).mkString("\n")
  }

  def extractModel(rdf: RDFAsJenaModel): Model = {
    rdf match {
      case rdfJena: RDFAsJenaModel => rdfJena.model
      case _ => throw new Exception("Cannot extract Model from rdf:" + rdf)
    }
  }

}