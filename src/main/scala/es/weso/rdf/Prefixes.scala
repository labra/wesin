package es.weso.rdf

import es.weso.rdfgraph.nodes.IRI

object PREFIXES {

  lazy val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
  lazy val rdf = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  lazy val owl = IRI("http://www.w3.org/2002/07/owl#")
  lazy val rdfs = IRI("http://www.w3.org/2000/01/rdf-schema#")

  lazy val rdf_langString = rdf.add("langString")
  lazy val xsd_string = xsd.add("string")
  lazy val xsd_integer = xsd.add("integer")
  lazy val xsd_double = xsd.add("double")
  lazy val xsd_decimal = xsd.add("decimal")
  lazy val xsd_boolean = xsd.add("boolean")
  lazy val rdf_type = rdf.add("type")

  private val shMap: Map[String, IRI] =
    Map("rdf" -> rdf, "xsd" -> xsd, "rdfs" -> rdfs, "owl" -> owl
    )

}