package org.weso.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scala.io.Source

import org.weso.rdfNode._
import org.weso.rdfTriple._
import scala.util.parsing.input.CharArrayReader



class TurtleParserSuite extends TurtleParser with FunSpec with ShouldMatchers {

  describe("Turtle Parser") {
   val parser=TurtleParser

   describe("turtleDoc") {
     implicit val s = ParserState.initial
     val p = parser.turtleDoc
     
     val a01 = (BNodeId(0),RDFNode.rdftype,BNodeId(1))
     val a02 = (BNodeId(0),RDFNode.rdftype,BNodeId(2))
     val abc = (IRI("a"),IRI("b"),IRI("c"))
     
     shouldParseRDF(p," _:0 a _:1,_:2 .",List(a01,a02))
     shouldParseRDF(p,"_:0 a _:1,_:2 .",List(a01,a02))
     shouldParseRDF(p,"_:0 a _:1; a _:2 .",List(a01,a02))
     shouldParseRDF(p,"<a> <b> <c> .",List(abc))
     shouldParseRDF(p,"# Example \n <a> <b> <c> .",List(abc))
     shouldParseRDF(p,"# No triples ",List())
   }

   
   describe("prefixId") {
     val p = parser.prefixId
     shouldParseGeneric(p,
                    "@prefix a: <http://example.org/a#>",
                   ("a:",IRI("http://example.org/a#")))
     shouldParseGeneric(p,
                    "@prefix : <http://example.org/a#>",
                   (":",IRI("http://example.org/a#")))
     shouldParseGeneric(p,
                    "@prefix año: <http://example.org/a#>",
                   ("año:",IRI("http://example.org/a#")))
   }
    
   describe("SPARQLPrefix") {
     val p = parser.SPARQLPrefix
     shouldParseGeneric(p,
                    "prefix a: <http://example.org/a#>",
                   ("a:",IRI("http://example.org/a#")))
     shouldParseGeneric(p,
                    "PREFIX : <http://example.org/a#>",
                   (":",IRI("http://example.org/a#")))
     shouldParseGeneric(p,
                    "Prefix año: <http://example.org/a#>",
                   ("año:",IRI("http://example.org/a#")))
   }

   describe("triples") {
     val state =
     	   ParserState.initial.
     	     addPrefix("a:", IRI("http://example.org/a#")).
     	     addCurPredicate(IRI("a")).
     	     addCurSubject(IRI("a"))
     val p = parser.triples(state)
     val a01 = (BNodeId(0),RDFNode.rdftype,BNodeId(1))
     val a02 = (BNodeId(0),RDFNode.rdftype,BNodeId(2))
     
     shouldParseRDF(p,"_:0 a _:1,_:2",List(a01,a02))
     shouldParseRDF(p,"_:0 a _:1; a _:2",List(a01,a02))
   }

   describe("subjPredicateObjectList") {
     val state =
     	   ParserState.initial.
     	     addPrefix("a:", IRI("http://example.org/a#")).
     	     addCurPredicate(IRI("a")).
     	     addCurSubject(IRI("a"))
     val p = parser.subjPredicatesObjectList(state)
     val a12 = (RDFNode.rdftype,List(BNodeId(1),BNodeId(2)))
     val a34 = (RDFNode.rdftype,List(BNodeId(3),BNodeId(4)))
     shouldParseRDF(p,"_:0 a _:1,_:2 ; a _:3, _:4",(BNodeId(0),List(a12,a34)))
   }

   describe("predicateObjectList") {
     val state =
     	   ParserState.initial.
     	     addPrefix("a:", IRI("http://example.org/a#")).
     	     addCurPredicate(IRI("a")).
     	     addCurSubject(IRI("a"))
     val p = parser.predicateObjectList(state)
     val a01 = (RDFNode.rdftype,List(BNodeId(0),BNodeId(1)))
     val a12 = (RDFNode.rdftype,List(IntegerLiteral(1),IntegerLiteral(2)))
     val a23 = (RDFNode.rdftype,List(BNodeId(2),BNodeId(3)))
     val abac1 = (IRI("http://example.org/a#b"),List(IRI("http://example.org/a#c"),IntegerLiteral(1)))
     val result = List(a01,abac1)

     shouldParseRDF(p,"a 1,2",List(a12))
     shouldParseRDF(p,"a 1,2;",List(a12))
     shouldParseRDF(p,"a 1,2;;",List(a12))
     shouldParseRDF(p,"a _:1,_:2",List(a01))
     shouldParseRDF(p,"a _:1,_:2 ;",List(a01))
     shouldParseRDF(p,"a _:1,_:2 ; a _:3, _:4",List(a01,a23))
     shouldParseRDF(p,"a _:1,_:2 ; ; a _:3, _:4 ;",List(a01,a23))
     shouldParseRDF(p,"a _:1,_:2;a:b a:c, 1 ",result)
     shouldParseRDF(p,"""|a _:1,_:2 ;
    		 		     |a:b a:c, 1 """.stripMargin, result) 
     }

   
   describe("verbObjectList") {
     val state =
     	   ParserState.initial.
     	     addPrefix("a:", IRI("http://example.org/a#")).
     	     addCurPredicate(IRI("a")).
     	     addCurSubject(IRI("a"))
     val p = parser.verbObjectList(state)
     shouldParseRDF(p,"a _:1,_:2",(RDFNode.rdftype,List(BNodeId(0),BNodeId(1)))) 
     shouldParseRDF(p," a _:1, _:2",(RDFNode.rdftype,List(BNodeId(0),BNodeId(1)))) 
     shouldParseRDF(p,"a:b _:1,_:2",(IRI("http://example.org/a#b"),List(BNodeId(0),BNodeId(1)))) 
     }

   describe("objectList") {
     val state =
     	   ParserState.initial.
     	     addPrefix("a:", IRI("http://example.org/a#")).
     	     addCurPredicate(IRI("a")).
     	     addCurSubject(IRI("a"))
     val p = parser.objectList(state)
     shouldParseRDF(p,"_:1,_:2",List(BNodeId(0),BNodeId(1))) 
     shouldParseRDF(p,"_:1,_:2,_:1",List(BNodeId(0),BNodeId(1),BNodeId(0))) 
     shouldParseRDF(p,"_:1,a:b,_:1",List(BNodeId(0),IRI("http://example.org/a#b"),BNodeId(0))) 
     shouldParseRDF(p,"1,2,3",List(IntegerLiteral(1),IntegerLiteral(2),IntegerLiteral(3))) 
     shouldParseRDF(p,"1, 2, 3",List(IntegerLiteral(1),IntegerLiteral(2),IntegerLiteral(3))) 
     shouldParseRDF(p,"1 , 2 , 3 ",List(IntegerLiteral(1),IntegerLiteral(2),IntegerLiteral(3))) 
     shouldParseRDF(p,"1 , true , false ",List(IntegerLiteral(1),RDFNode.trueLiteral,RDFNode.falseLiteral)) 
     shouldParseRDF(p,"#Comment\n 1 , true # Other comment\n , false ",List(IntegerLiteral(1),RDFNode.trueLiteral,RDFNode.falseLiteral)) 
     shouldParseRDF(p,"_:1, a:b, _:1",List(BNodeId(0),IRI("http://example.org/a#b"),BNodeId(0))) 
     }
     
   describe("rdf_object") {
     val state =
     	   ParserState.initial.addPrefix("a:", IRI("http://example.org/a#"))
     val p = parser.rdf_object(state)
     shouldParseRDF(p,"\"Hi\"",StringLiteral("Hi")) 
     shouldParseRDF(p,"2",IntegerLiteral(2)) 
     shouldParseRDF(p,"2.3",DecimalLiteral(2.3)) 
     shouldParseRDF(p,"2.3e-1",DoubleLiteral(2.3e-1)) 
     shouldParseRDF(p,"-2.3e-1",DoubleLiteral(-2.3e-1)) 
     shouldParseRDF(p,"a:b",IRI("http://example.org/a#b")) 
     shouldParseRDF(p,"<http://example.org/a#b>",IRI("http://example.org/a#b")) 
     shouldParseRDF(p,"_:1",BNodeId(0)) 
     shouldParseRDF(p,"[]",BNodeId(0)) 
    }
   
   describe("literal") {
     val prefixMap =
           PrefixMap.addPrefix("a:",IRI("http://example.org/a#"))(
     	   PrefixMap.addPrefix(":",IRI("http://example.org#"))(
     	   PrefixMap.addPrefix("año:",IRI("http://example.org/año#"))(
     	   PrefixMap.empty)))
     val p = parser.literal(prefixMap)
     shouldParseGeneric(p,"1.2",DecimalLiteral(1.2))
     shouldParseGeneric(p,"12",IntegerLiteral(12))
     shouldParseGeneric(p,"1.2e9",DoubleLiteral(1.2e09))
     shouldParseGeneric(p,"\"a\"^^a:b",DatatypeLiteral("a",IRI("http://example.org/a#b")))
     shouldParseGeneric(p,"\"a\"",StringLiteral("a"))
     shouldParseGeneric(p,"\"a\"@es",LangLiteral("a",Lang("es")))
   }

   
   describe("blankNodePropertyList") {
     
   }

   describe("collection") {
     
   }

   describe("NumericLiteral") {
     val p = parser.NumericLiteral
     shouldParseGeneric(p,"1.2",DecimalLiteral(1.2))
     shouldParseGeneric(p,"12",IntegerLiteral(12))
     shouldParseGeneric(p,"1.2e9",DoubleLiteral(1.2e09))
     shouldParseGeneric(p,"-1.2e9",DoubleLiteral(-1.2e09))
   }

   describe("RDFLiteral") {
     val prefixMap =
           PrefixMap.addPrefix("a:",IRI("http://example.org/a#"))(
     	   PrefixMap.addPrefix(":",IRI("http://example.org#"))(
     	   PrefixMap.addPrefix("año:",IRI("http://example.org/año#"))(
     	   PrefixMap.empty)))
     val p = parser.RDFLiteral(prefixMap)
	 shouldParseGeneric(p,"\"123\"^^a:integer",DatatypeLiteral("123",IRI("http://example.org/a#integer")))
	 shouldParseGeneric(p,"\"123\"^^<http://example.org/a#integer>",DatatypeLiteral("123",IRI("http://example.org/a#integer")))
	 shouldParseGeneric(p,"\"\"\"John Doe\"\"\"",StringLiteral("John Doe"))
	 shouldParseGeneric(p,"\"one\"@es",LangLiteral("one",Lang("es")))
	 shouldParseGeneric(p,"\"one\"",StringLiteral("one"))
	 shouldParseGeneric(p,"\"one two\"",StringLiteral("one two"))
	 shouldParseGeneric(p,"\"\"\"John \nDoe\"\"\"",StringLiteral("John \nDoe"))
	 shouldParseGeneric(p,"\'\'\'John \nDoe\'\'\'",StringLiteral("John \nDoe"))
	 shouldNotParse(p,".")
   }

   describe("BooleanLiteral") {
     val p = parser.BooleanLiteral
     shouldParseGeneric(p,"true",RDFNode.trueLiteral)
	 shouldParseGeneric(p,"false",RDFNode.falseLiteral)
	 shouldNotParse(p,"tres")
	 shouldNotParse(p,"one")
   }

	 
	 describe("string") {
     val p = parser.string
     shouldParseGeneric(p,"\"hi\"","hi")
	 shouldParseGeneric(p,"'Hi'","Hi")
	 shouldParseGeneric(p,"'''Hi'John'''","Hi\'John")
	 shouldNotParse(p,"3.2")
   }

   describe("iri") {
     val prefixMap =
           PrefixMap.addPrefix("a:",IRI("http://example.org/a#"))(
     	   PrefixMap.addPrefix(":",IRI("http://example.org#"))(
     	   PrefixMap.addPrefix("año:",IRI("http://example.org/año#"))(
     	   PrefixMap.empty)))
     val p = parser.iri(prefixMap)
     shouldParseGeneric(p,":a",IRI("http://example.org#a"))
	 shouldParseGeneric(p,"a:b",IRI("http://example.org/a#b"))
	 shouldParseGeneric(p,"a:",IRI("http://example.org/a#"))
	 shouldParseGeneric(p,"<http://a.com>",IRI("http://a.com"))
	 shouldNotParse(p,"3.2")
   }

   describe("PrefixedName") {
     val prefixMap =
           PrefixMap.addPrefix("a:",IRI("http://example.org/a#"))(
     	   PrefixMap.addPrefix(":",IRI("http://example.org#"))(
     	   PrefixMap.addPrefix("año:",IRI("http://example.org/año#"))(
     	   PrefixMap.empty)))     
     val p = parser.PrefixedName(prefixMap)
     shouldParseGeneric(p,":a",IRI("http://example.org#a"))
	 shouldParseGeneric(p,"a:b",IRI("http://example.org/a#b"))
	 shouldParseGeneric(p,"a:",IRI("http://example.org/a#"))
	 shouldNotParse(p,"<a>")
   } 

   describe("BlankNode") {
     val bNodeTable = BNodeTable.empty
     val table1 = bNodeTable.getOrAddBNode("1")
     val tableA = bNodeTable.getOrAddBNode("a")
     val tableAnon = bNodeTable.newBNode
     val p = parser.BlankNode(bNodeTable)
     shouldParseRDF(p,"_:1",BNodeId(0))
	 shouldParseRDF(p,"_:a",BNodeId(0))
	 shouldParseRDF(p,"[]",BNodeId(0))
	 shouldNotParse(p,"<a>")
   }

   describe("IRIREF_STR") {
     val p = IRIREF_STR.r
     shouldParse(p,"<>")
	 shouldParse(p,"<hi>")
	 shouldParse(p,"<http://pepe.com>")
	 shouldParse(p,"<urn://kiko-uno>")
	 shouldNotParse(p,"<a>.")
	 shouldNotParse(p,"<a\"b>")
	 shouldNotParse(p,"<a{a}b>")
   }

   describe("PNAME_NS_STR") {
     val p = PNAME_NS_STR.r
     shouldParse(p,"a:")
	 shouldParse(p,":")
	 shouldParse(p,"año:")
	 shouldNotParse(p,"a:b")
   }

   describe("PNAME_NS") {
     val prefixMap =
           PrefixMap.addPrefix("a:",IRI("http://example.org/a#"))(
     	   PrefixMap.addPrefix(":",IRI("http://example.org#"))(
     	   PrefixMap.addPrefix("año:",IRI("http://example.org/año#"))(
     	   PrefixMap.empty)))
     val p = parser.PNAME_NS(prefixMap)
     shouldParseGeneric(p,"a:",IRI("http://example.org/a#"))
	 shouldParseGeneric(p,":",IRI("http://example.org#"))
	 shouldParseGeneric(p,"año:",IRI("http://example.org/año#"))
	 shouldNotParse(p,"a:b")
   }

   describe("PNAME_LN") {
     val prefixMap =
           PrefixMap.addPrefix("a:",IRI("http://example.org/a#"))(
     	   PrefixMap.addPrefix(":",IRI("http://example.org#"))(
     	   PrefixMap.addPrefix("año:",IRI("http://example.org/año#"))(
     	   PrefixMap.empty)))
     val p = parser.PNAME_LN(prefixMap)
     shouldParseGeneric(p,"a:b",IRI("http://example.org/a#b"))
	 shouldParseGeneric(p,"a:aa",IRI("http://example.org/a#aa"))
	 shouldParseGeneric(p,"a:año",IRI("http://example.org/a#año"))
	 shouldParseGeneric(p,"a:a\\~a",IRI("http://example.org/a#a~a"))
	 shouldNotParse(p,"a:año ")
   }


   describe("BLANK_NODE_LABEL") {
     val bNodeTable = BNodeTable.empty
     val p = parser.BLANK_NODE_LABEL(bNodeTable)
     shouldParseGen(p,"_:1")
	 shouldParseGen(p,"_:aa")
	 shouldParseGen(p,"_:año")
	 shouldParseGen(p,"_:1.2a")
	 shouldParseGen(p,"_:1.a")
	 shouldNotParse(p,"_:1 a .")
   }

   describe("BLANK_NODE_LABEL_STR") {
     val p = BLANK_NODE_LABEL_STR.r
     shouldParse(p,"_:1")
	 shouldParse(p,"_:aa")
	 shouldParse(p,"_:año")
// fails?	 shouldParse(p,"_:1.2")
	 shouldParse(p,"_:1.2a")
	 shouldParse(p,"_:1.a")
	 shouldNotParse(p,"_:1 a .")
   }

   describe("LANGTAG") {
     val p = parser.LANGTAG
	 shouldParseGeneric(p,"@es",Lang("es"))
	 shouldParseGeneric(p,"@es-am",Lang("es-am"))
	 shouldParseGeneric(p,"@am-am2-23",Lang("am-am2-23"))
	 shouldNotParse(p,"es")
	 shouldNotParse(p,"@12")
	 shouldNotParse(p,"@año")
   }

   describe("INTEGER") {
     val p = parser.INTEGER
	 shouldParseGeneric(p,"2",IntegerLiteral(2))
	 shouldParseGeneric(p,"-3",IntegerLiteral(-3))
	 shouldParseGeneric(p,"-33",IntegerLiteral(-33))
	 shouldNotParse(p,"3.4")
	 shouldNotParse(p,"334.")
	 shouldNotParse(p,"-3.34E+98")
   }

	 describe("DECIMAL") {
     val p = parser.DECIMAL
	 shouldParseGeneric(p,"2.34",DecimalLiteral(2.34))
	 shouldParseGeneric(p,"-3.0",DecimalLiteral(-3.0))
	 shouldParseGeneric(p,"-3.3",DecimalLiteral(-3.3))
	 shouldNotParse(p,"3")
	 shouldNotParse(p,"334")
	 shouldNotParse(p,"-3.34E+98")
	 shouldNotParse(p,"-334")
   }

   describe("DOUBLE") {
     val p = parser.DOUBLE
	 shouldParseGeneric(p,"2.34e98",DoubleLiteral(2.34e98))
	 shouldParseGeneric(p,"-3E+98",DoubleLiteral(-3E98))
	 shouldParseGeneric(p,"-3.34E+98",DoubleLiteral(-3.34E+98))
	 shouldNotParse(p,"-3.34EE+98")
	 shouldNotParse(p,"-3.34E +98")
   }

   describe("EXPONENT") {
     val p = EXPONENT.r
	 shouldParse(p,"e9876")
	 shouldParse(p,"E+98")
	 shouldParse(p,"E-98")
	 shouldParse(p,"e-98")
	 shouldNotParse(p,"e++")
	 shouldNotParse(p,"e+a")
	 shouldNotParse(p,"E+-98")
   }

   describe("STRING_LITERAL_QUOTE_STR") {
     val p = STRING_LITERAL_QUOTE_STR.r
	 shouldParse(p,"\"Hello\"")
	 shouldParse(p,"\"\\u0123\\t\\u00AF\"")
	 shouldNotParse(p,"\"Ho\"la\"")
   }

   describe("STRING_LITERAL_QUOTE") {
     val p = parser.STRING_LITERAL_QUOTE
	 shouldParseGeneric(p,"\"Hello\"","Hello")
	 shouldParseGeneric(p,"\"\\u0123\\t\\u00AF\"","\u0123\t\u00AF")
	 shouldNotParse(p,"\"Ho\"la\"")
   }

   describe("STRING_LITERAL_SINGLE_QUOTE") {
     val p = parser.STRING_LITERAL_SINGLE_QUOTE
	 shouldParseGeneric(p,"'Hello'","Hello")
	 shouldParseGeneric(p,"'\\u0123'","\u0123")
	 shouldNotParse(p,"'Ho'la'")
   }

   describe("STRING_LITERAL_LONG_SINGLE_QUOTE") {
     val p = parser.STRING_LITERAL_LONG_SINGLE_QUOTE
	 shouldParseGeneric(p,"'''Hello'''","Hello")
	 shouldParseGeneric(p,"'''\\u0123'''","\u0123")
	 shouldParseGeneric(p,"'''H e l l o . '''","H e l l o . ")
	 shouldParseGeneric(p,"'''Hi 'John' . '''","Hi \'John\' . ")
	 shouldParseGeneric(p,"'''Hi'John'''","Hi\'John")
	 shouldParseGeneric(p,"'''Hi \"John\" . '''","Hi \"John\" . ")
	 shouldParseGeneric(p,"'''Hi \"\"John\"\" . '''","Hi \"\"John\"\" . ")
	 shouldParseGeneric(p,"'''Hi\nJohn'''","Hi\nJohn")
	 shouldNotParse(p,"\"\"\"Hi \"\"\"John\"\" . \"\"\"")
   }

   describe("STRING_LITERAL_LONG_QUOTE") {
     val p = parser.STRING_LITERAL_LONG_QUOTE
	 shouldParseGeneric(p,"\"\"\"Hello\"\"\"","Hello")
	 shouldParseGeneric(p,"\"\"\"\\u0123\"\"\"","\u0123")
	 shouldParseGeneric(p,"\"\"\"H e l l o . \"\"\"","H e l l o . ")
	 shouldParseGeneric(p,"\"\"\"Hi \"John\" . \"\"\"","Hi \"John\" . ")
	 shouldParseGeneric(p,"\"\"\"Hi \'John\' . \"\"\"","Hi \'John\' . ")
	 shouldParseGeneric(p,"\"\"\"Hi \"\"John\"\" . \"\"\"","Hi \"\"John\"\" . ")
	 shouldNotParse(p,"\"\"\"Hi \"\"\"John\"\" . \"\"\"")
	 
   }

   describe("UCHAR_Parser") {
     val p = parser.UCHAR_Parser
	 shouldParseGeneric(p,"\\u002e",'.')
	 shouldParseGeneric(p,"\\U00002e",'.')
	 shouldNotParse(p,"\\x00002e")
   }
   
   describe("UCHAR") {
     val p = UCHAR.r
	 shouldParse(p,"\\u0123")
	 shouldParse(p,"\\u01AF")
	 shouldParse(p,"\\U0123AD")
	 shouldNotParse(p,"\\x0123AD")
	 shouldNotParse(p,"\\u0123AD")
	 shouldNotParse(p,"\\U0123")
	 shouldNotParse(p,"\\u012H")
   }

   describe("ECHAR_Parser") {
     val p = parser.ECHAR_Parser
	 shouldParseGeneric(p,"\\t",'\t')
	 shouldParseGeneric(p,"\\n",'\n')
	 shouldNotParse(p,"\\a")
	 shouldNotParse(p,"a")
   }
   
   describe("PN_CHARS_BASE") {
     val p = parser.PN_CHARS_BASE_Parser
	 shouldParseGeneric(p,"x",'x')
	 shouldParseGeneric(p,"�",'�')
   }

   describe("ECHAR") {
     val p = ECHAR.r
	 shouldParse(p,"\\t")
	 shouldParse(p,"\\b")
	 shouldParse(p,"\\n")
	 shouldParse(p,"\\r")
	 shouldParse(p,"\\f")
	 shouldParse(p,"\\\"")
	 shouldNotParse(p,"a")
   }

   describe("WS_STR") {
     val p = WS_STR.r
	 shouldParse(p," ")
	 shouldNotParse(p,"a")
   }

   describe("WS") {
     val p = parser.WS
	 shouldParseGen(p," ")
     shouldParseGen(p,"    ")
     shouldParseGen(p,"  \n  ")
	 shouldParseGen(p,"  # comment \n")
	 shouldNotParse(p,"a")
   }

   describe("ANON_STR") {
     val p = ANON_STR.r
	 shouldParse(p,"[]")
	 shouldParse(p,"[ ]")
	 shouldParse(p,"[ \t]")
	 shouldParse(p,"[ \n]")
	 shouldNotParse(p,"[a]")
   }

   describe("ANON") {
     val bNodeTable = BNodeTable.empty
     val p = parser.ANON(bNodeTable)
	 shouldParseGen(p,"[]")
	 shouldParseGen(p,"[ ]")
	 shouldParseGen(p,"[ \t]")
	 shouldParseGen(p,"[ \n]")
	 shouldNotParse(p,"[a]")
   }

   describe("PN_CHARS_BASE") {
     val p = PN_CHARS_BASE.r
	 shouldParse(p,"A")
	 shouldParse(p,"Ñ")
	 shouldParse(p,"\u2088")
	 shouldNotParse(p, "\u2001")
   }

   describe("PN_CHARS") {
     val p = PN_CHARS.r
	 shouldParse(p,"A")
	 shouldParse(p,"-")
	 shouldParse(p,"7")
	 shouldParse(p,"\u00B7")
	 shouldNotParse(p, ":")
   }


   describe("PN_PREFIX") {
     val p = PN_PREFIX.r
	 shouldParse(p,"A")
	 shouldParse(p,"a.o")
	 shouldParse(p,"a..o")
	 shouldParse(p,"a.�o")
	 shouldParse(p,"a.x.o")
//	 shouldParse(p,"a-1")
//	 shouldParse(p,"a--1")
	 shouldParse(p,"año")
     shouldParse(p,"AA")
     shouldParse(p, "b.a")
	 shouldNotParse(p, "b:a")
   }

   
   describe("PN_LOCAL") {
     val p = PN_LOCAL.r
	 shouldParse(p,"A�o")
	 shouldParse(p,":a")
	 shouldParse(p,":89")
	 shouldParse(p,":a�o_88")
	 shouldParse(p,":a�o:88")
	 shouldNotParse(p,":a�88 .")
	 shouldNotParse(p,":a�88,")
   }

   describe("PLX") {
     val p = PLX.r
	 shouldParse(p,"%12")
	 shouldParse(p,"\\-")
   }

   describe("PERCENT") {
     val p = PERCENT.r
	 shouldParse(p,"%12")
	 shouldParse(p,"%9F")
	 shouldParse(p,"%FF")
	 shouldNotParse(p, "AAA")
	 shouldNotParse(p, "%%")
	 shouldNotParse(p, "%GH")
   }

   
   describe("PN_LOCAL_ESC") {
     val p = PN_LOCAL_ESC.r
     shouldParse(p,"\\-")
     shouldParse(p,"\\!")
     shouldNotParse(p,"a")
   }
   
   describe("UCHAR2Char") {
     it("Should parse unicode with 4 hex") {
       UCHAR2char("\\u002e") === '.'
     }
     it("Should parse unicode with 6 hex") {
       UCHAR2char("\\U00002e") === '.'
     }
   }

   describe("ECHAR2Char") {
     it("Should parse tab") {
       ECHAR2char("\\t") === '\t'
     }
     it("Should parse newline") {
       ECHAR2char("\\n") === '\n'
     }
   }

   def shouldParse(p:TurtleParser.Parser[String], s : String) {
     shouldParseGeneric(p,s,s)
   }

   // Only checks if parser succeeds
   def shouldParseGen[A](p:TurtleParser.Parser[A], s : String) {
    it("Should parse \"" + s + "\"") {
      val result = parser.parseAll(p,s) match {
        case parser.Success(x,_) => true 
        case parser.NoSuccess(msg,_) => fail(msg)
      }
    }
   }

    def shouldParseGeneric[A](p:TurtleParser.Parser[A], s : String, a : A) {
    it("Should parse \"" + s + "\"" + " and return " + a.toString) {
      val result = parser.parseAll(p,s) match {
        case parser.Success(x,_) => x 
        case parser.NoSuccess(msg,_) => fail(msg)
      }
      result should be(a)
    }
   }
 
    def shouldParseRDF[A](p:TurtleParser.Parser[A], s : String, a : A) {
    it("Should parse \"" + s + "\"" + " and return " + a.toString) {
      val result = parser.parseAll(p,s) match {
        case parser.Success((x,_),_) => x 
        case parser.NoSuccess(msg,_) => fail(msg)
      }
      result should be(a)
    }
   }
 
    def shouldNotParse[A](p:TurtleParser.Parser[A], s : String) {
    it("Should not parse \"" + s + "\"") {
      val result = parser.parseAll(p,s) match {
        case parser.Success(x,_) => fail("Should not parse " + s + ", but parsed value " + x) 
        case parser.NoSuccess(msg,_) => success(msg)
      }
    }
   }
  
  }
}