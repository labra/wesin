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

sealed trait Literal extends RDFNode {
  def lexicalForm : String
  def dataType : IRI
  
  def isLangLiteral : Boolean
  def hasLang(lang : Lang) : Boolean
}

case class DatatypeLiteral(lexicalForm : String, dataType : IRI) extends Literal {
  override def isLangLiteral = false
  override def hasLang(lang : Lang) = false
  override def toString : String = {
    "\"" + lexicalForm + "\"^^" + dataType
  }
}

// It should be better to inherit from DatatypeLiteral, 
// but case-to-case inheritance is prohibited in Scala
case class IntegerLiteral(int: Integer) extends Literal {
  val dataType = RDFNode.IntegerDatatypeIRI
  val lexicalForm = int.toString
  
  override def isLangLiteral = false
  override def hasLang(lang:Lang) = false
  
  override def toString : String = {
    lexicalForm 
  }
  
}

case class DecimalLiteral(decimal: BigDecimal) extends Literal {
  val dataType = RDFNode.DecimalDatatypeIRI
  val lexicalForm = decimal.toString
  
  override def isLangLiteral = false
  override def hasLang(lang:Lang) = false
  
  override def toString : String = {
    lexicalForm 
  }
  
}

case class DoubleLiteral(double: Double) extends Literal {
  val dataType = RDFNode.DoubleDatatypeIRI
  val lexicalForm = double.toString
  
  override def isLangLiteral = false
  override def hasLang(lang:Lang) = false
  
  override def toString : String = {
    lexicalForm 
  }
  
}

case class StringLiteral(lexicalForm: String) extends Literal {
  val dataType = RDFNode.StringDatatypeIRI
    
  override def isLangLiteral = false
  override def hasLang(lang:Lang) = false
  
  override def toString : String = {
    lexicalForm 
  }
  
}


case class BooleanLiteral(bool: Boolean) extends Literal {
  val dataType = RDFNode.BooleanDatatypeIRI
  val lexicalForm = if (bool) "true" else "false"
    
  override def isLangLiteral = false
  override def hasLang(lang:Lang) = false
  
  override def toString : String = {
    lexicalForm 
  }
  
}

case class LangLiteral(lexicalForm: String, lang : Lang) extends Literal {
  val dataType = RDFNode.LangStringDatatypeIRI
  
  def isLangLiteral = true
  def hasLang(l : Lang) = lang.matchLanguage(l)

  override def toString : String = {
    val lex = "\"" + lexicalForm + "\"" 
    lex + lang
  }
}

case class Lang(lang : String) {
  
  // This should be the right regular expression for lang.
  // We don't use this expression because the specification does not also.
  val langtag_ex : String = "(\\A[xX]([\\x2d]\\p{Alnum}{1,8})*\\z)" +
       "|(((\\A\\p{Alpha}{2,8}(?=\\x2d|\\z)){1}" +
       "(([\\x2d]\\p{Alpha}{3})(?=\\x2d|\\z)){0,3}" +
       "([\\x2d]\\p{Alpha}{4}(?=\\x2d|\\z))?" +
       "([\\x2d](\\p{Alpha}{2}|\\d{3})(?=\\x2d|\\z))?" +
       "([\\x2d](\\d\\p{Alnum}{3}|\\p{Alnum}{5,8})(?=\\x2d|\\z))*)" +
       "(([\\x2d]([a-wyzA-WYZ](?=\\x2d))([\\x2d](\\p{Alnum}{2,8})+)*))*" +
       "([\\x2d][xX]([\\x2d]\\p{Alnum}{1,8})*)?)\\z" 

  // TODO. Specification defines other ways to match languages
  def matchLanguage(other : Lang) = 
    this.lang.toLowerCase == other.lang.toLowerCase
       
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

object RDFNode {
 val xsd					= "http://www.w3.org/2001/XMLSchema#"
 val rdfSyntax				= "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
 val StringDatatypeIRI 		= IRI(xsd + "string") 
 val LangStringDatatypeIRI  = IRI(rdfSyntax + "langString")
 val BooleanDatatypeIRI 	= IRI(xsd + "boolean")
 val IntegerDatatypeIRI 	= IRI(xsd + "integer")
 val DoubleDatatypeIRI 		= IRI(xsd + "double")
 val DecimalDatatypeIRI 	= IRI(xsd + "decimal")
 val rdftype				= IRI(rdfSyntax + "type")
 
 val trueLiteral = BooleanLiteral(true)
 val falseLiteral = BooleanLiteral(false)
 
 def qNameIRI(prefix:IRI, local:String) : IRI = {
   IRI(prefix.str + local)
 }

}
