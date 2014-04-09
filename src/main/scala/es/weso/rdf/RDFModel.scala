package es.weso.rdf

import es.weso.rdfgraph.statements._
import es.weso.rdfgraph.RDFGraph
import es.weso.parser.PrefixMap
import es.weso.parser.TurtleParser
import scala.util.Try
import es.weso.rdfgraph.nodes.InitialBNodeId

case class RDFModel(graph: RDFGraph, pm: PrefixMap) {
 
 override def toString(): String = {
   graph.show(InitialBNodeId)
 }
}

object RDFModel {
  
  /**
   * parse a string and obtain an RDF graph
   */
  def parse(cs: CharSequence): Try[RDFModel] = {
   for ((triples,pm) <- TurtleParser.parse(cs)) 
      yield RDFModel(RDFGraph.fromTriples(triples),pm)
  }
}