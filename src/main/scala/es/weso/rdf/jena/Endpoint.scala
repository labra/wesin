package es.weso.rdf.jena

import com.hp.hpl.jena.query._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.nodes.RDFNode
import es.weso.rdfgraph.statements.RDFTriple
import scala.collection.JavaConversions._
import scala.collection.immutable.StringOps._
import scala.util.Try
import es.weso.rdfgraph.statements._
import com.hp.hpl.jena.rdf.model.{ RDFNode => JenaRDFNode }
import com.hp.hpl.jena.rdf.model.Property
import com.hp.hpl.jena.rdf.model.Statement
import com.hp.hpl.jena.rdf.model.Model
import org.slf4j._
import com.hp.hpl.jena.rdf.model.{ RDFNode => JenaRDFNode }
import es.weso.rdf._
import es.weso.rdf.jena.SPARQLQueries._
import com.hp.hpl.jena.rdf.model.{ RDFNode => JenaRDFNode }
import com.hp.hpl.jena.rdf.model.{ RDFNode => JenaRDFNode }

case class Endpoint(endpoint: String) extends RDFReader {
  // TODO: check that endpoint is a well formed URI
  type Rdf = Endpoint

  override def getPrefixMap: PrefixMap = {
    // TODO: Can we get more info about prefix maps from an endpoint?
    PrefixMap(Map())
  }

  val log = LoggerFactory.getLogger("Endpoint")

  override def parse(cs: CharSequence, format: String): Try[Endpoint] = {
    throw new Exception("Cannot parse into an endpoint. endpoint = " + endpoint)
  }

  override def serialize(format: String): String = {
    throw new Exception("Cannot serialize an endpoint. endpoint = " + endpoint)
  }

  override def iris(): Set[IRI] = {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findIRIs).execSelect()
    resultSet.map(qs => IRI(qs.get("x").asResource.getURI)).toSet
  }

  override def subjects(): Set[RDFNode] = {
    // TODO: The following code only returns resource IRIs (no BNodes)
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findIRIs).execSelect()
    resultSet.map(qs => IRI(qs.get("x").asResource.getURI)).toSet
  }

  override def predicates(): Set[IRI] = {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findIRIs).execSelect()
    resultSet.map(qs => IRI(qs.get("p").asResource.getURI)).toSet
  }

  override def objects(): Set[IRI] = {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findIRIs).execSelect()
    resultSet.map(qs => IRI(qs.get("y").asResource.getURI)).toSet
  }

  def rdfTriples(): Set[RDFTriple] = {
    val model = QueryExecutionFactory.sparqlService(endpoint, queryTriples).execConstruct()
    model2triples(model)
  }

  def triplesWithSubject(node: RDFNode): Set[RDFTriple] = {
    if (node.isIRI) {
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithSubject(node.toIRI)).execConstruct()
      model2triples(model)
    } else throw new Exception("triplesWithSubject: node " + node + " must be a IRI")
  }

  def triplesWithPredicate(p: IRI): Set[RDFTriple] = {
    val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithPredicate(p)).execConstruct()
    model2triples(model)
  }

  def triplesWithObject(node: RDFNode): Set[RDFTriple] = {
    if (node.isIRI) {
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithObject(node.toIRI)).execConstruct()
      model2triples(model)
    } else throw new Exception("triplesWithObject: node " + node + " must be a IRI")
  }

  def triplesWithPredicateObject(p: IRI, o: RDFNode): Set[RDFTriple] = {
    if (o.isIRI) {
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithPredicateObject(p, o.toIRI)).execConstruct()
      model2triples(model)
    } else throw new Exception("triplesWithPredicateObject: o " + o + " must be a IRI")
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
      BNodeId(r.asNode.getBlankNodeId.getLabelString)
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

}