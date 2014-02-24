package es.weso.rdfGraph

import es.weso.rdfNode._
import es.weso.rdfTriple._
import es.weso.tgraph._
import es.weso.tgraph.TGraph
import es.weso.tgraph.TGraphImpl

import scala.collection.Set
import scala.collection.immutable.Map
import scalax.collection.immutable.Graph
import scalax.collection.GraphEdge._

/**
 * Type of exceptions raised
 */
case class RDFGraphException(val msg: String) extends Exception

abstract class RDFGraph {
  
  def isEmpty : Boolean
  
  /** 
   * insertTriple inserts a triple into a graph
   * Blank nodes inside the triple may conflict with graph blank nodes 
   */
  def insertTriple(triple: RDFTriple) : RDFGraph 
  
  /**
   * addTriples adds a set of triples
   * 
   * Blank nodes in the set of triples are renamed
   * 
   */
  def addTriples(triples : Set[RDFTriple]) : RDFGraph 
  
  /** 
   * triples returns the set of triples of a graph
   * 
   * @param seed Initial BNode identifier to start renaming
   */
  def triples(implicit seed : BNodeId) : Set[RDFTriple]
  
  /**
   * set of IRIs of this RDFGraph
   */
  def IRIs : Set[IRI]

  /**
   * Merges this graph with a new one
   */
  def merge (other : RDFGraph) : RDFGraph

  /**
   * addTriplesBNodes adds triples to this graph 
   * 
   * @param triples set of triples to insert
   * @param map maps from BNodeIds in triples to BNodeIds in graph
   */
  def addTriplesBNodes(bnodes : Set[BNodeId], 
		  triples : Set[RDFTriple],
		  map : Map[BNodeId,BNodeId]) : RDFGraph 
		  
  /**
   * insertTripleMap inserts a triple in this graph
   * 
   * @param triple to insert
   * @param map maps from BNodeIds in triple to BNodeIds in Graph
   * 
   */
  def insertTripleMap(triple: RDFTriple,
		  			  map : Map[BNodeId,BNodeId]) : RDFGraph 
  
  def show(implicit seed : BNodeId) : String
  
 
  def foldRDFGraph[A] 
		(e: A, 
		 f: (TContext[RDFNode],A) => A)
  		(implicit seed : BNodeId) : A = {
    foldRDFGraphSeed(e,f,seed)
  }

  def foldRDFGraphSeed[A] (
		  e: A, 
		  fn: (TContext[RDFNode],A) => A, 
		  seed : BNodeId) : A 

 def foldRDFGraphOrd[A] 
		 (e: A, 
		  f: (TContext[RDFNode],A) => A)
         (implicit ord : Ordering[RDFNode], 
        		   seed : BNodeId) : A = {
    foldRDFGraphSeedOrd(e,f,seed)(ord)
  } 

  def foldRDFGraphSeedOrd[A] (
		  e: A, 
		  fn: (TContext[RDFNode],A) => A,
		  seed : BNodeId)
		  (implicit ord : Ordering[RDFNode]) : A 
  
  //  def isomorphic(other : RDFGraph) : Boolean

  
}

case class Ground(val graph : TGraph[RDFNode])
                 (implicit seed : BNodeId) extends RDFGraph {

  override def isEmpty = graph.isEmpty
  

  override def insertTriple(triple: RDFTriple) : RDFGraph = {
     Ground(graph.addTriple(triple.subj,triple.pred,triple.obj))
  }
  
  def getBNodeMap(node : RDFNode, map: Map[BNodeId,BNodeId]) : RDFNode = {
    node match {
      case b@BNodeId(_) => map(b)
      case other => other
    }
  }

  override def insertTripleMap(triple: RDFTriple,
		  			  map : Map[BNodeId,BNodeId]) : RDFGraph = {
     val s = getBNodeMap(triple.subj,map)
     val p = triple.pred
     val o = getBNodeMap(triple.obj,map)
     Ground(graph.addTriple(s,p,o))
  }

  
  override def addTriplesBNodes(
		  bnodes : Set[BNodeId], 
		  triples : Set[RDFTriple],
		  map : Map[BNodeId,BNodeId]) : RDFGraph = {
    if (bnodes.isEmpty){
      val current : RDFGraph = this
      triples.foldLeft (current) ((g,triple) => 
        						g.insertTripleMap(triple,map))
    } else {
      Exists((bnode) => addTriplesBNodes(bnodes.tail,
    		  					triples,
    		  					map + (bnodes.head -> bnode) )) 
    }
  }

  /**
   * addTriples inserts a set of triples into a graph 
   * 
   * It takes control of possible bnodes in the triples renaming them 
   * @param triples set of triples
   * 
   */
  override def addTriples(triples : Set[RDFTriple]) : RDFGraph = {
    val bnodes = RDFTriple.collectBNodes(triples)
    addTriplesBNodes(bnodes,triples,Map.empty)
  }
  
  override def IRIs : Set[IRI] = {
    graph.nodes.filter(_.isIRI).map(_.toIRI)
  }
  
  /** get the triples of a graph
   * 
   * @param seed represents the seed for blank node identifier generation
   * (default value = 0)
   * 
   * Ground graphs ignore the seed parameter
   * 
   */
  override def triples(implicit seed : BNodeId) : Set[RDFTriple] = {
    graph.triples.map((t) => RDFTriple(t._1,t._2.toIRI,t._3))
  }  
  
  override def merge (other: RDFGraph) : RDFGraph = {
    val g = this.addTriples(other.triples)
    g
  }
  
  override def show (implicit seed : BNodeId): String = {
    this.toString
  }

  /**
   * Decompose a graph from a given IRI
   * @param node resource from which we are decomposing the graph
   * 
   * @author labra
   */
  def decomp (node : IRI) : Option[(TContext[RDFNode],RDFGraph)] = {
    graph.decomp(node) match {
      case None => None
      case Some((ctx,g)) => Some((ctx,Ground(g)))
    }
  }
  
  override def foldRDFGraphSeed[A] (e: A, f:(TContext[RDFNode],A) => A, seed : BNodeId) : A = {
    graph.foldTGraph(e)(f)
  }

  def foldRDFGraphSeedOrd[A] (
		  e: A, 
		  f: (TContext[RDFNode],A) => A,
		  seed : BNodeId)
		  (implicit ord : Ordering[RDFNode]) : A = {
    graph.foldTGraphOrd(e)(f)(ord)
  }
    
}

case class Exists(fn : BNodeId => RDFGraph)
				   (implicit seed : BNodeId) extends RDFGraph {

  override def isEmpty = false
  
  override def insertTriple(triple: RDFTriple) : RDFGraph = {
    Exists((bnode) => (fn(bnode)).insertTriple(triple))
  }
  
  override def addTriples(triples : Set[RDFTriple]) : RDFGraph = {
    Exists((bnode) => (fn(bnode)).addTriples(triples))
  }

  override def triples (implicit seed : BNodeId): Set[RDFTriple] = {
   (fn(seed)).triples(seed.newBNodeId)  
  }

  override def IRIs: Set[IRI] = {
   (fn(seed)).IRIs  
  }

  /*
   * merges this graph with another one
   */
  override def merge (other : RDFGraph) : RDFGraph = {
    Exists((bnode => (fn(bnode)).merge(other)))
  }

  /*
   * add triples which can have a set of bNodes
   */
  override def addTriplesBNodes(bnodes : Set[BNodeId], 
		  triples : Set[RDFTriple],
		  map : Map[BNodeId,BNodeId]) : RDFGraph = {
    Exists((bnode) => (fn(bnode)).addTriplesBNodes(bnodes,triples,map))
  }
  
  override def insertTripleMap(triple: RDFTriple,
		  			  map : Map[BNodeId,BNodeId]) : RDFGraph = {
    Exists((bnode) => (fn(bnode)).insertTripleMap(triple,map))
  }
  
  override def show(implicit seed : BNodeId) : String = {
    "Exists " + seed.id + " ( " + (fn(seed)).show(seed.newBNodeId) + ")"
  }
  
  override def foldRDFGraphSeed[A] (e: A, f: (TContext[RDFNode],A) => A, seed: BNodeId) : A = {
    (fn(seed)).foldRDFGraphSeed(e,f,seed.newBNodeId)
  }

  def foldRDFGraphSeedOrd[A] (
		  e: A, 
		  f: (TContext[RDFNode],A) => A,
		  seed : BNodeId)
		  (implicit ord : Ordering[RDFNode]) : A = {
    (fn(seed)).foldRDFGraphSeedOrd(e,f,seed.newBNodeId)(ord)
  }
  
}



object RDFGraph {

  // Implicit definition for starting BNodeId
  // Should I put it in other place?
  implicit val initialBNode = BNodeId(0)

 /**
  * Empty RDF Graph (no nodes and edges)
  */
 def empty : RDFGraph = Ground(new TGraphImpl(Graph[RDFNode,Triple]()))
 
 /**
  * Context represents the context of an IRI in a RDF Graph
  * 
  * @param iri iri from which this context is about
  * @param succ list of succesors (outgoing edges): pairs of (property,object)
  * @param pred list of predecessors (incoming edges): pairs of (subject,property)
  * 
  */
		   		    
  def showFolds (g : RDFGraph) : String = {
    g.foldRDFGraph(
         "\n", 
         ((ctx : TContext[RDFNode],r : String) => "ctx: " + ctx + "\n" + r )
         )
  }

  implicit def minOrd = new Ordering[IRI] { 
    def compare(a: IRI, b: IRI) = b.str compare a.str 
  }
 
  def showFoldsOrd (g : RDFGraph) : String = {
    implicit val ord = new Ordering[RDFNode] { 
      def compare(x :RDFNode,y :RDFNode): Int = 
        (x,y) match {
          case (a : IRI, b : IRI) => a.uri.compareTo(b.uri)
          case (_ : IRI,BNodeId(_)) => -1
          case (_ : IRI,_ : Literal) => -1
          case (_ : BNodeId, _ : IRI) => 1
          case (_ : Literal, _ : IRI) => 1
          case (BNodeId(a),BNodeId(b)) => a.compare(b)
          case (l1:Literal,l2:Literal) => 0 // TODO: Maybe we should compare literals by their lexical form
          case (BNodeId(_),l:Literal) => 1 // TODO: Comparing literals and BNodes could be irrelevant...maybe 0 ?
          case (l:Literal,BNodeId(_)) => -1
          case _ => throw new RDFNodeException("Unexpected values " + (x,y) + " comparing RDFNodes")
       }
   	}
   	g.foldRDFGraphOrd("", ((ctx : TContext[RDFNode],r : String) => "ctx: " + ctx + "\n" + r ))
  }


}
