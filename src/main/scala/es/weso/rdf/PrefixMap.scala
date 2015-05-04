package es.weso.rdf

import es.weso.rdfgraph.nodes._
import scala.collection.immutable.Map

case class PrefixMap(pm: Map[String, IRI]) {

  def getIRI(prefix: String): Option[IRI] = {
    pm.get(prefix)
  }

  def qname(str: String): Option[IRI] = {
    str.indexOf(":") match {
      case (-1) => Some(IRI(str))
      case n => {
        val (alias, localname) = str.splitAt(n)
        getIRI(alias).map(iri => iri.add(localname))
      }
    }
  }

  def contains(prefix: String): Boolean = pm.contains(prefix)

  def addPrefix(prefix: String, iri: IRI): PrefixMap = {
    PrefixMap(pm + (prefix -> iri))
  }

  override def toString: String = {
    def cnv(pair: (String, IRI)): String = {
      pair._1 + ": <" + pair._2 + ">\n"
    }
    pm.map(cnv).mkString("\n")
  }
}

object PrefixMap {
  def empty = PrefixMap(Map[String, IRI]())

  def addPrefix(prefix: String, iri: IRI)(pm: PrefixMap): PrefixMap =
    pm.addPrefix(prefix, iri)

}

