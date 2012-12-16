package org.weso.rdfNode

case class RDFNodeException(msg : String) extends Exception

sealed class RDFNode {
  def isIRI = this match {
    case IRI(_) => true
    case _ => false
  }
  def toIRI = this match {
    case i@IRI(_) => i
    case _ => throw RDFNodeException("Cannot convert RDFNode " + this + " to IRI")
  }
}

case class BNodeId(id : Int) extends RDFNode {
  def newBNodeId : BNodeId = BNodeId(id + 1)
  
  override def toString : String = {
    "_:b" + id
  }
  
}

object InitialBNodeId extends BNodeId(0)


trait Literal extends RDFNode {
  def lexicalForm : String
  def dataType : IRI
  
  def isLangLiteral : Boolean
  def hasLang(lang : Lang) : Boolean
}

case class DatatypeLiteral(lexicalForm : String, dataType : IRI) extends Literal {
  def hasLang(lang : Lang) = false

  def isLangLiteral = false
  
  override def toString : String = {
    "\"" + lexicalForm + "\"^^" + dataType
  }
}

case class LangLiteral(lexicalForm: String, lang : Lang) extends Literal {
  val dataType = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")

  def isLangLiteral = true
  def hasLang(l : Lang) = lang.matchLanguage(l)

  override def toString : String = {
    val lex = "\"" + lexicalForm + "\"" 
    lex + lang
  }
}

case class Lang(lang : String) {
  val langtag_ex : String = "(\\A[xX]([\\x2d]\\p{Alnum}{1,8})*\\z)" +
       "|(((\\A\\p{Alpha}{2,8}(?=\\x2d|\\z)){1}" +
       "(([\\x2d]\\p{Alpha}{3})(?=\\x2d|\\z)){0,3}" +
       "([\\x2d]\\p{Alpha}{4}(?=\\x2d|\\z))?" +
       "([\\x2d](\\p{Alpha}{2}|\\d{3})(?=\\x2d|\\z))?" +
       "([\\x2d](\\d\\p{Alnum}{3}|\\p{Alnum}{5,8})(?=\\x2d|\\z))*)" +
       "(([\\x2d]([a-wyzA-WYZ](?=\\x2d))([\\x2d](\\p{Alnum}{2,8})+)*))*" +
       "([\\x2d][xX]([\\x2d]\\p{Alnum}{1,8})*)?)\\z" 

  // TODO. Specification defines other ways to match languages
  def matchLanguage(other : Lang) = lang == other.lang
  
  override def toString = lang match {
    case "" => ""
    case ls => "@" + ls
  }
  
}

case class IRI(str : String) extends RDFNode {
  override def toString = {
    "<" + str + ">"
  }

  implicit def minOrd = new Ordering[IRI] { 
    def compare(a: IRI, b: IRI) = b.str compare a.str 
  }
}

