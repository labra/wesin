package es.weso.wesin

import es.weso.rdfGraph
import es.weso.rdfGraph._
import es.weso.rdfNode._
import es.weso.rdfTriple._

object Wesin extends App {

 override def main(args: Array[String]) = {
   implicit val seed = BNodeId(0)
   println("Inductive RDF")
   val e = RDFGraph.empty
   val g = e.addTriples(Set(
		   RDFTriple(IRI("1"),IRI("2"),IRI("3")),
		   RDFTriple(IRI("1"),IRI("2"),IRI("4")),
		   RDFTriple(IRI("2"),IRI("5"),IRI("6")),
           RDFTriple(BNodeId(0),IRI("5"),IRI("6")),
           RDFTriple(IRI("1"),IRI("2"),BNodeId(1)),
           RDFTriple(BNodeId(1),IRI("2"),BNodeId(2)),
           RDFTriple(BNodeId(2),IRI("2"),BNodeId(3)),
           RDFTriple(BNodeId(3),IRI("2"),IRI("1"))
          ))
   println("Graph: " + g.show)
   println("Triples: ")
   for {
     t <- g.triples
   } println(t)
   println("Folds: ")
   print(RDFGraph.showFolds(g))
   println("Folds ordered: ")
   print(RDFGraph.showFoldsOrd(g))
 }

}

