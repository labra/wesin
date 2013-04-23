package org.weso.graph

case class Context[A](
		node : A, 
		pred: Set[(A,A)], 
		succ: Set[(A,A)], 
		rels: Set[(A,A)]
) {
  
  def triples : Set[(A,A,A)] = {
    val emptyTriples = Set[(A,A,A)]()
    succ.foldLeft(emptyTriples) { (r,p) => Set((node,p._1,p._2)) ++ r } ++ 
    pred.foldLeft(emptyTriples) { (r,p) => Set((p._1,p._2,node)) ++ r } ++ 
    rels.foldLeft(emptyTriples) { (r,p) => Set((p._1,node,p._2)) ++ r }
   }
  
}
