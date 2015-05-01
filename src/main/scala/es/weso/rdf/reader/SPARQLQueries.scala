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
import com.hp.hpl.jena.rdf.model.Property
import com.hp.hpl.jena.rdf.model.Statement
import com.hp.hpl.jena.rdf.model.Model
import org.slf4j._
import com.hp.hpl.jena.rdf.model.{ RDFNode => JenaRDFNode }
import org.apache.jena.riot.RDFDataMgr
import com.hp.hpl.jena.rdf.model.ModelFactory
import es.weso.rdf._

object SPARQLQueries {

  def queryTriples() = {
    QueryFactory.create(
      s"""|construct {?x ?p ?y } where {
         |?x ?p ?y .
       |}
         |""".stripMargin
    )
  }

  def queryTriplesWithSubject(subj: IRI) = {
    val s = subj.str
    QueryFactory.create(
      s"""|construct {<${s}> ?p ?y } where {
         |<${s}> ?p ?y .
       |}
         |""".stripMargin
    )
  }

  def queryTriplesWithObject(obj: IRI) = {
    val s = obj.str
    QueryFactory.create(
      s"""|construct {?x ?p <${s}> } where {
         | ?x ?p <${s}> .
       |}
         |""".stripMargin
    )
  }

  def queryTriplesWithPredicateObject(p: IRI, o: IRI) = {
    QueryFactory.create(
      s"""|construct {?x <${p.str}> <${o.str}> } where {
          | ?x <${p.str}> <${o.str}> .
          |}
          |""".stripMargin
    )
  }

  lazy val findIRIs = QueryFactory.create(
    """|select ?x where {
         | ?x ?p ?y .
         | filter (isIRI(?x))
       |}
         |""".stripMargin
  )

  lazy val findRDFTriples = QueryFactory.create(
    """|construct { ?x ?p ?y } where {
         | ?x ?p ?y .
       |}
         |""".stripMargin
  )

  lazy val findSubjects = QueryFactory.create(
    """|select ?x where {
         | ?x ?p ?y .
         | filter (isIRI(?x))
       |}
         |""".stripMargin
  )

  lazy val findPredicates = QueryFactory.create(
    """|select ?p where {
         | ?x ?p ?y .
         | filter (isIRI(?p))
       |}
         |""".stripMargin
  )

  lazy val findObjects = QueryFactory.create(
    """|select ?y where {
         | ?x ?p ?y .
         | filter (isIRI(?y))
       |}
         |""".stripMargin
  )

}