package es.weso.rdf.jena

import org.scalatest.FunSpec
import com.hp.hpl.jena.rdf.model.ModelFactory
import java.io.ByteArrayInputStream
import com.hp.hpl.jena.rdf.model.Model
import java.io.InputStream
import org.scalatest.Matchers
import es.weso.rdf.nodes._
import es.weso.rdf.jena.JenaMapper._
import es.weso.rdf.triples.RDFTriple

trait JenaBased extends Matchers {

  def shouldBeIsomorphic(m1: Model, m2: Model): Unit = {
    val b = m1.isIsomorphicWith(m2)
    if (!b) {
      println("Models are not isomorphic")
      println("-------------- Model 1:" + m1.toString)
      println("-------------- Model 2:" + m2.toString)
    }
    b should be(true)
  }

  def str2model(s: String): Model = {
    val m = ModelFactory.createDefaultModel
    val in: InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))
    m.read(in, "", "TURTLE")
    m
  }

}