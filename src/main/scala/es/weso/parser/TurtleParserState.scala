package es.weso.parser

import es.weso.rdfgraph.nodes._
import scala.language.postfixOps
import es.weso.rdfgraph.statements.RDFTriple

case class TurtleParserState (  triples : List[RDFTriple],  namespaces : PrefixMap ,  bNodeLabels : BNodeTable,  baseIRI: IRI  ) {

  def addTriple (t : RDFTriple) : TurtleParserState = {
    TurtleParserState(t :: triples, namespaces, bNodeLabels, baseIRI)
  }

  def addTriples (ts : List[RDFTriple]) : TurtleParserState = {
    TurtleParserState(ts ++ triples, namespaces, bNodeLabels, baseIRI)
  }

  def retrieveTriples: (List[RDFTriple], TurtleParserState) = {
   (triples, TurtleParserState(List(), namespaces, bNodeLabels, baseIRI))
  }

 def newTable (table: BNodeTable) : TurtleParserState = 
   TurtleParserState(triples,namespaces,table,baseIRI)
   
 def addPrefix(prefix: String, iri: IRI) : TurtleParserState = 
   TurtleParserState(triples,namespaces.addPrefix(prefix, iri),bNodeLabels,baseIRI)

 def newBNode : (BNodeId,TurtleParserState) = { 
   val (id,t) = bNodeLabels.newBNode ; 
   (id,TurtleParserState(triples,namespaces,t,baseIRI))
 }
 
 def newBase(newIRI:IRI) =
   TurtleParserState(triples,namespaces,bNodeLabels,newIRI)

}

object TurtleParserState {
  
  def initial : TurtleParserState = initial(IRI(""))
  def initial(baseIRI : IRI) = TurtleParserState(List(),PrefixMap.empty,BNodeTable.empty,baseIRI)
  
}