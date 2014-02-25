package es.weso.rdfgraph.nodes

import java.net.{URISyntaxException, URI}



case class IRI(uri : URI) extends RDFNode {

  override def toString = {
    "<" + uri.toString + ">"
  }

  implicit def minOrd = new Ordering[IRI] {
    def compare(a: IRI, b: IRI) = a.uri.compareTo(b.uri)
  }

  def str : String = {
    uri.toString
  }

  /**
   * Resolve an IRI against this IRI (which is taken as the base)
   * Currently, we employ java.net.URI algorithm to resolve
   */
  def resolve(iri: IRI): IRI = {
    IRI(uri.resolve(iri.uri))
  }



}

object IRI {
  def apply(str:String):IRI = {
    // Todo: Capture exceptions to provide better error messages?
    IRI(new URI(str))
  }

  def unapply(str:String):Option[IRI] = {
    try {
      Some(IRI(new URI(str)))
    } catch {
      case _ : URISyntaxException => None
    }
  }

}