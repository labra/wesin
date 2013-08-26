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

class W3cTokensSuite 
	extends W3cTokens
	with TestParser
	with FunSpec 
	with ShouldMatchers {

  describe("W3c Tokens") {

   describe("IRIREF") {
     val p = IRIREF
     shouldParseGeneric(p,"<>",IRI(""))
	 shouldParseGeneric(p,"<hi>",IRI("hi"))
	 shouldParseGeneric(p,"<http://pepe.com>",IRI("http://pepe.com"))
	 shouldParseGeneric(p,"<urn://kiko-uno>",IRI("urn://kiko-uno"))
	 shouldParseGeneric(p,"http://a.example/\\U00000073",IRI("http://a.example/s"))
	 shouldNotParse(p,"<a>.")
	 shouldNotParse(p,"<a\"b>")
	 shouldNotParse(p,"<a{a}b>")
   }

   describe("IRIREF_STR") {
     val p = IRIREF_STR.r
     shouldParse(p,"<>")
	 shouldParse(p,"<hi>")
	 shouldParse(p,"<http://pepe.com>")
	 shouldParse(p,"<urn://kiko-uno>")
	 shouldParse(p,"""http://a.example/\U00000073""")
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
     val p = PNAME_NS(prefixMap)
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
     val p = PNAME_LN(prefixMap)
     shouldParseGeneric(p,"a:b",IRI("http://example.org/a#b"))
	 shouldParseGeneric(p,"a:aa",IRI("http://example.org/a#aa"))
	 shouldParseGeneric(p,"a:año",IRI("http://example.org/a#año"))
	 shouldParseGeneric(p,"a:a\\~a",IRI("http://example.org/a#a~a"))
	 shouldNotParse(p,"a:año ")
   }


   describe("BLANK_NODE_LABEL") {
     val bNodeTable = BNodeTable.empty
     val p = BLANK_NODE_LABEL(bNodeTable)
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
     val p = LANGTAG
	 shouldParseGeneric(p,"@es",Lang("es"))
	 shouldParseGeneric(p,"@es-am",Lang("es-am"))
	 shouldParseGeneric(p,"@am-am2-23",Lang("am-am2-23"))
	 shouldNotParse(p,"es")
	 shouldNotParse(p,"@12")
	 shouldNotParse(p,"@año")
   }

   describe("INTEGER") {
     val p = INTEGER
	 shouldParseGeneric(p,"2",IntegerLiteral(2))
	 shouldParseGeneric(p,"-3",IntegerLiteral(-3))
	 shouldParseGeneric(p,"-33",IntegerLiteral(-33))
	 shouldNotParse(p,"3.4")
	 shouldNotParse(p,"334.")
	 shouldNotParse(p,"-3.34E+98")
   }

	 describe("DECIMAL") {
     val p = DECIMAL
	 shouldParseGeneric(p,"2.34",DecimalLiteral(2.34))
	 shouldParseGeneric(p,"-3.0",DecimalLiteral(-3.0))
	 shouldParseGeneric(p,"-3.3",DecimalLiteral(-3.3))
	 shouldNotParse(p,"3")
	 shouldNotParse(p,"334")
	 shouldNotParse(p,"-3.34E+98")
	 shouldNotParse(p,"-334")
   }

   describe("DOUBLE") {
     val p = DOUBLE
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
     val p = STRING_LITERAL_QUOTE
	 shouldParseGeneric(p,"\"Hello\"","Hello")
	 shouldParseGeneric(p,"\"\\u0123\\t\\u00AF\"","\u0123\t\u00AF")
	 shouldNotParse(p,"\"Ho\"la\"")
   }

   describe("STRING_LITERAL_SINGLE_QUOTE") {
     val p = STRING_LITERAL_SINGLE_QUOTE
	 shouldParseGeneric(p,"'Hello'","Hello")
	 shouldParseGeneric(p,"'\\u0123'","\u0123")
	 shouldParseGeneric(p,"'\\\\'","\\")
	 shouldNotParse(p,"'Ho'la'")
   }

   describe("STRING_LITERAL_LONG_SINGLE_QUOTE") {
     val p = STRING_LITERAL_LONG_SINGLE_QUOTE
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
     val p = STRING_LITERAL_LONG_QUOTE
	 shouldParseGeneric(p,"\"\"\"Hello\"\"\"","Hello")
	 shouldParseGeneric(p,"\"\"\"\\u0123\"\"\"","\u0123")
	 shouldParseGeneric(p,"\"\"\"H e l l o . \"\"\"","H e l l o . ")
	 shouldParseGeneric(p,"\"\"\"Hi \"John\" . \"\"\"","Hi \"John\" . ")
	 shouldParseGeneric(p,"\"\"\"Hi \'John\' . \"\"\"","Hi \'John\' . ")
	 shouldParseGeneric(p,"\"\"\"Hi \"\"John\"\" . \"\"\"","Hi \"\"John\"\" . ")
	 shouldNotParse(p,"\"\"\"Hi \"\"\"John\"\" . \"\"\"")
	 
   }

   describe("UCHAR_Parser") {
     val p = UCHAR_Parser
	 shouldParseGeneric(p,"\\u002e",'.')
	 shouldParseGeneric(p,"\\U0000002e",'.')
	 shouldNotParse(p,"\\x00002e")
   }
   
   describe("UCHAR") {
     val p = UCHAR_STR.r
	 shouldParse(p,"\\u0123")
	 shouldParse(p,"\\u01AF")
	 shouldParse(p,"\\U0123AD00")
	 shouldNotParse(p,"\\x0123AD")
	 shouldNotParse(p,"\\u0123AD")
	 shouldNotParse(p,"\\U0123")
	 shouldNotParse(p,"\\u012H")
   }

   describe("ECHAR_Parser") {
     val p = ECHAR_Parser
	 shouldParseGeneric(p,"\\t",'\t')
	 shouldParseGeneric(p,"\\n",'\n')
	 shouldNotParse(p,"\\a")
	 shouldNotParse(p,"a")
   }
   
   describe("PN_CHARS_BASE") {
     val p = PN_CHARS_BASE_Parser
	 shouldParseGeneric(p,"x",'x')
	 shouldParseGeneric(p,"�",'�')
   }

   describe("ECHAR") {
     val p = ECHAR_STR.r
	 shouldParse(p,"\\t")
	 shouldParse(p,"\\b")
	 shouldParse(p,"\\n")
	 shouldParse(p,"\\r")
	 shouldParse(p,"\\f")
	 shouldParse(p,"\\\"")
	 shouldParse(p,"\\\\")
	 shouldNotParse(p,"a")
   }

   describe("WS_STR") {
     val p = WS_STR.r
	 shouldParse(p," ")
	 shouldNotParse(p,"a")
   }

   describe("WS") {
     val p = WS
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
     val p = ANON(bNodeTable)
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

   /** 
    *  This is the same as shouldParseGeneric but 
    *  ignores the state
    */
    def shouldParseRDF[A](p:Parser[A], s : String, a : A) {
    it("Should parse \"" + s + "\"" + " and return " + a.toString) {
      val result = parseAll(p,s) match {
        case Success((x,_),_) => x 
        case NoSuccess(msg,_) => fail(msg)
      }
      result should be(a)
    }
   }

 }
}