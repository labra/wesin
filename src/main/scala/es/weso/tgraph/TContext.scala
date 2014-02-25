package es.weso.tgraph

/*
triple context, part of inductive graph approach, useful for graph folding
 */
case class TContext[A](
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
  
  def map[B](f : A => B): TContext[B] = {
    TContext(f(node),mapPairs(f,pred),mapPairs(f,succ),mapPairs(f,rels))
  }
  
  def mapPairs[A,B](f: A => B, ls : Set[(A,A)]): Set[(B,B)] = {
    ls.map((x:(A,A)) => (f(x._1),f(x._2)))
  }

}
