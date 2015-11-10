package es.weso.parser

import org.scalatest.{ Matchers, FunSpec }
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdf.PrefixMap

class TurtleParserSuite
    extends FunSpec
    with TurtleParser
    with TestParser
    with Matchers {

  describe("Turtle Parser") {

    // Example from https://github.com/labra/rdfshape/issues/16
    describe("String + LangTag") {
      val expected = Set(RDFTriple(IRI("a"), IRI("b"), LangLiteral("hi", Lang("es"))))
      val baseIRI = IRI("")
      shouldParseTurtle("<a> <b> \"hi\"@es .", baseIRI, expected)

      // In the grammar it seems that there cannot be whitespace 
      // between string literals and language tags 
      shouldNotParseTurtle("<a> <b> \"hi\" @es .")
    }

    describe("turtleDoc") {
      implicit val s = TurtleParserState.initial
      val a01 = RDFTriple(BNodeId("0"), RDFNode.rdftype, BNodeId("1"))
      val a02 = RDFTriple(BNodeId("0"), RDFNode.rdftype, BNodeId("2"))
      val abc = RDFTriple(IRI("a"), IRI("b"), IRI("c"))
      val baseIRI = IRI("")
      shouldParseTurtle(
        """|@base <http://example.org/>.
            |<a> <b> <c> .""".stripMargin,
        baseIRI,
        Set(RDFTriple(
          IRI("http://example.org/a"),
          IRI("http://example.org/b"),
          IRI("http://example.org/c")
        ))
      )
      shouldParseTurtle(" _:0 a _:1,_:2 .", baseIRI, Set(a01, a02))
      shouldParseTurtle("_:0 a _:1,_:2 .", baseIRI, Set(a01, a02))
      shouldParseTurtle("_:0 a _:1; a _:2 .", baseIRI, Set(a01, a02))
      shouldParseTurtle("<a> <b> <c> .", baseIRI, Set(abc))
      shouldParseTurtle("# Example \n <a> <b> <c> .", baseIRI, Set(abc))
      shouldParseTurtle("# No triples ", baseIRI, Set())
      shouldParseTurtle(
        """|@prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.""".stripMargin,
        baseIRI,
        Set()
      )
      shouldParseTurtle(
        """|@prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            |""".stripMargin,
        baseIRI, Set()
      )
      shouldParseTurtle(
        """|@prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            |@prefix xsd:     <http://www.w3.org/2001/XMLSchema#> .
            |<s> <p> "123"^^xsd:string .""".stripMargin,
        baseIRI, Set(RDFTriple(
        IRI("s"),
        IRI("p"),
        DatatypeLiteral("123", IRI("http://www.w3.org/2001/XMLSchema#string"))
      ))
      )
      shouldNotParseTurtle("""|# Bad IRI : good escape, bad charcater
        |<http://www.w3.org/2013/TurtleTests/\u0020> <http://www.w3.org/2013/TurtleTests/p> <http://www.w3.org/2013/TurtleTests/o> .""".stripMargin)

      shouldNotParseTurtle("<a{b}> <b> <c> .")
      shouldNotParseTurtle("<a\u0020> <b> <c> .")
      shouldNotParseTurtle("<a> = <b> .")
    }

    describe("baseId") {
      val p = baseId
      shouldParseGeneric(
        p,
        "@base <http://example.org/a#> .",
        IRI("http://example.org/a#")
      )
      shouldParseGeneric(
        p,
        "@base <http://example.org/>.",
        IRI("http://example.org/")
      )
      shouldParseGeneric(
        p,
        "@base <http://example.org/año#>  .",
        IRI("http://example.org/año#")
      )
    }

    describe("prefixId") {
      val p = prefixId
      shouldParseGeneric(
        p,
        "@prefix a: <http://example.org/a#> .",
        ("a", IRI("http://example.org/a#"))
      )
      shouldParseGeneric(
        p,
        "@prefix : <http://example.org/a#>.",
        ("", IRI("http://example.org/a#"))
      )
      shouldParseGeneric(
        p,
        "@prefix año: <http://example.org/a#> \n.",
        ("año", IRI("http://example.org/a#"))
      )
    }

    describe("SPARQLPrefix") {
      val p = SPARQLPrefix
      shouldParseGeneric(
        p,
        "prefix a: <http://example.org/a#>",
        ("a", IRI("http://example.org/a#"))
      )
      shouldParseGeneric(
        p,
        "PREFIX : <http://example.org/a#>",
        ("", IRI("http://example.org/a#"))
      )
      shouldParseGeneric(
        p,
        "Prefix año: <http://example.org/a#>",
        ("año", IRI("http://example.org/a#"))
      )
    }

    describe("triples") {
      val state =
        TurtleParserState.initial.
          addPrefix("a", IRI("http://example.org/a#"))
      val p = triples(state)
      val b0 = BNodeId("b" + 0)
      val ap = IRI("http://example.org/a#p")
      val aq = IRI("http://example.org/a#p")
      val ab = IRI("http://example.org/a#b")
      val a01 = RDFTriple(BNodeId("0"), RDFNode.rdftype, BNodeId("1"))
      val a02 = RDFTriple(BNodeId("0"), RDFNode.rdftype, BNodeId("2"))

      shouldParseRDF(p, "_:0 a _:1,_:2", List(a01, a02))
      shouldParseRDF(p, "_:0 a _:1; a _:2", List(a01, a02))
      shouldParseRDF(p, "[a:p a:b ]", List(RDFTriple(BNodeId("b0"), ap, ab)))

      // TODO: Review why this test fails
      shouldParseRDF(p, "_:b.0 a _:1",
        List(RDFTriple(BNodeId("b:0"), RDFNode.rdftype, BNodeId("1"))))

      shouldNotParse(p, "<a> = <b>")
      shouldNotParse(p, " ")
    }

    describe("subjPredicateObjectList") {
      val state =
        TurtleParserState.initial.
          addPrefix("a", IRI("http://example.org/a#"))
      val p = subjPredicatesObjectList(state)
      val a12 = (RDFNode.rdftype, List(BNodeId("1"), BNodeId("2")))
      val a34 = (RDFNode.rdftype, List(BNodeId("3"), BNodeId("4")))
      shouldParseRDF(p, "_:0 a _:1,_:2 ; a _:3, _:4", (BNodeId("0"), List(a12, a34)))
    }

    describe("predicateObjectList") {
      val state =
        TurtleParserState.initial.
          addPrefix("a", IRI("http://example.org/a#"))

      val p = predicateObjectList(state)
      val a01 = (RDFNode.rdftype, List(BNodeId("0"), BNodeId("1")))
      val a12 = (RDFNode.rdftype, List(IntegerLiteral(1), IntegerLiteral(2)))
      val a_12 = (RDFNode.rdftype, List(BNodeId("1"), BNodeId("2")))
      val a_23 = (RDFNode.rdftype, List(BNodeId("2"), BNodeId("3")))
      val a_34 = (RDFNode.rdftype, List(BNodeId("3"), BNodeId("4")))
      val abac1 = (IRI("http://example.org/a#b"), List(IRI("http://example.org/a#c"), IntegerLiteral(1)))
      val result = List(a_12, abac1)

      shouldParseRDF(p, "a 1,2", List(a12))
      shouldParseRDF(p, "a 1,2;", List(a12))
      shouldParseRDF(p, "a 1,2;;", List(a12))
      shouldParseRDF(p, "a _:1,_:2", List(a_12))
      shouldParseRDF(p, "a _:1,_:2 ;", List(a_12))
      shouldParseRDF(p, "a _:1,_:2 ; a _:3, _:4", List(a_12, a_34))
      shouldParseRDF(p, "a _:1,_:2 ; ; a _:3, _:4 ;", List(a_12, a_34))
      shouldParseRDF(p, "a _:1,_:2;a:b a:c, 1 ", result)
      shouldParseRDF(p, """|a _:1,_:2 ;
    		 		         |a:b a:c, 1 """.stripMargin, result)
    }

    describe("verbObjectList") {
      val state =
        TurtleParserState.initial.
          addPrefix("a", IRI("http://example.org/a#"))
      val p = verbObjectList(state)
      shouldParseRDF(p, "a _:1,_:2", (RDFNode.rdftype, List(BNodeId("1"), BNodeId("2"))))
      shouldParseRDF(p, " a _:1, _:2", (RDFNode.rdftype, List(BNodeId("1"), BNodeId("2"))))
      shouldParseRDF(p, "a:b _:1,_:2", (IRI("http://example.org/a#b"), List(BNodeId("1"), BNodeId("2"))))
    }

    describe("objectList") {
      val state =
        TurtleParserState.initial.
          addPrefix("a", IRI("http://example.org/a#"))
      val p = objectList(state)
      shouldParseRDF(p, "_:1,_:2", List(BNodeId("1"), BNodeId("2")))
      shouldParseRDF(p, "_:1,_:2,_:1", List(BNodeId("1"), BNodeId("2"), BNodeId("1")))
      shouldParseRDF(p, "_:1,a:b,_:1", List(BNodeId("1"), IRI("http://example.org/a#b"), BNodeId("1")))
      shouldParseRDF(p, "1,2,3", List(IntegerLiteral(1), IntegerLiteral(2), IntegerLiteral(3)))
      shouldParseRDF(p, "1, 2, 3", List(IntegerLiteral(1), IntegerLiteral(2), IntegerLiteral(3)))
      shouldParseRDF(p, "1 , 2 , 3 ", List(IntegerLiteral(1), IntegerLiteral(2), IntegerLiteral(3)))
      shouldParseRDF(p, "1 , true , false ", List(IntegerLiteral(1), RDFNode.trueLiteral, RDFNode.falseLiteral))
      shouldParseRDF(p, "#Comment\n 1 , true # Other comment\n , false ", List(IntegerLiteral(1), RDFNode.trueLiteral, RDFNode.falseLiteral))
      shouldParseRDF(p, "_:1, a:b, _:1", List(BNodeId("1"), IRI("http://example.org/a#b"), BNodeId("1")))
    }

    describe("rdf_object") {
      val state =
        TurtleParserState.initial.addPrefix("a", IRI("http://example.org/a#"))
      val p = rdf_object(state)
      shouldParseRDF(p, "\"Hi\"", StringLiteral("Hi"))
      shouldParseRDF(p, "2", IntegerLiteral(2))
      shouldParseRDF(p, "2.3", DecimalLiteral(2.3))
      shouldParseRDF(p, "2.3e-1", DoubleLiteral(2.3e-1))
      shouldParseRDF(p, "-2.3e-1", DoubleLiteral(-2.3e-1))
      shouldParseRDF(p, "a:b", IRI("http://example.org/a#b"))
      shouldParseRDF(p, "<http://example.org/a#b>", IRI("http://example.org/a#b"))
      shouldParseRDF(p, "_:1", BNodeId("1"))
      shouldParseRDF(p, "[]", BNodeId("b" + 0))
      shouldNotParse(p, "<http://www.w3.org/2013/TurtleTests/{abc}>")
    }

    describe("literal") {
      val prefixMap =
        PrefixMap.addPrefix("a", IRI("http://example.org/a#"))(
          PrefixMap.addPrefix("", IRI("http://example.org#"))(
            PrefixMap.addPrefix("año", IRI("http://example.org/año#"))(
              PrefixMap.empty
            )
          )
        )
      val p = literal(prefixMap)
      shouldParseGeneric(p, "1.2", DecimalLiteral(1.2))
      shouldParseGeneric(p, "12", IntegerLiteral(12))
      shouldParseGeneric(p, "1.2e9", DoubleLiteral(1.2e09))
      shouldParseGeneric(p, "\"a\"^^a:b", DatatypeLiteral("a", IRI("http://example.org/a#b")))
      shouldParseGeneric(p, "\"a\"", StringLiteral("a"))
      shouldParseGeneric(p, "\"a\"@es", LangLiteral("a", Lang("es")))
    }

    describe("blankNodePropertyList") {

    }

    describe("collection") {
      val s = TurtleParserState.initial
      val p = collection(s)
      shouldParseRDF(p, "( )", RDFNode.rdfnil)
      shouldParseRDF(p, "( <a> )", BNodeId("b" + 0))
      shouldParseRDF(p, "( <a> <b> )", BNodeId("b" + 1))
    }

    describe("NumericLiteral") {
      val p = NumericLiteral
      shouldParseGeneric(p, "1.2", DecimalLiteral(1.2))
      shouldParseGeneric(p, "12", IntegerLiteral(12))
      shouldParseGeneric(p, "1.2e9", DoubleLiteral(1.2e09))
      shouldParseGeneric(p, "-1.2e9", DoubleLiteral(-1.2e09))
    }

    describe("RDFLiteral") {
      val prefixMap =
        PrefixMap.addPrefix("a", IRI("http://example.org/a#"))(
          PrefixMap.addPrefix("", IRI("http://example.org#"))(
            PrefixMap.addPrefix("año", IRI("http://example.org/año#"))(
              PrefixMap.empty
            )
          )
        )
      val p = RDFLiteral(prefixMap)
      shouldParseGeneric(p, "\"123\"^^a:integer", DatatypeLiteral("123", IRI("http://example.org/a#integer")))
      shouldParseGeneric(p, "\"123\"^^<http://example.org/a#integer>", DatatypeLiteral("123", IRI("http://example.org/a#integer")))
      shouldParseGeneric(p, "\"\"\"John Doe\"\"\"", StringLiteral("John Doe"))
      shouldParseGeneric(p, "\"one\"@es", LangLiteral("one", Lang("es")))
      shouldParseGeneric(p, "\"one\"", StringLiteral("one"))
      shouldParseGeneric(p, "\"one two\"", StringLiteral("one two"))
      shouldParseGeneric(p, "\"\"\"John \nDoe\"\"\"", StringLiteral("John \nDoe"))
      shouldParseGeneric(p, "\'\'\'John \nDoe\'\'\'", StringLiteral("John \nDoe"))
      shouldNotParse(p, ".")
    }

    describe("BooleanLiteral") {
      val p = BooleanLiteral
      shouldParseGeneric(p, "true", RDFNode.trueLiteral)
      shouldParseGeneric(p, "false", RDFNode.falseLiteral)
      shouldNotParse(p, "tres")
      shouldNotParse(p, "one")
    }

    describe("string") {
      val p = string
      shouldParseGeneric(p, "\"hi\"", "hi")
      shouldParseGeneric(p, "'Hi'", "Hi")
      shouldParseGeneric(p, "'''Hi'John'''", "Hi\'John")
      shouldNotParse(p, "3.2")
    }

    describe("iri") {
      val prefixMap =
        PrefixMap.addPrefix("a", IRI("http://example.org/a#"))(
          PrefixMap.addPrefix("", IRI("http://example.org#"))(
            PrefixMap.addPrefix("año", IRI("http://example.org/año#"))(
              PrefixMap.empty
            )
          )
        )
      val p = iri(prefixMap)
      shouldParseGeneric(p, ":a", IRI("http://example.org#a"))
      shouldParseGeneric(p, "a:b", IRI("http://example.org/a#b"))
      shouldParseGeneric(p, "a:", IRI("http://example.org/a#"))
      shouldParseGeneric(p, "<http://a.com>", IRI("http://a.com"))
      shouldNotParse(p, "3.2")
      shouldNotParse(p, "<http://www.w3.org/2013/TurtleTests/{abc}>")
    }

    describe("PrefixedName") {
      val prefixMap =
        PrefixMap.addPrefix("a", IRI("http://example.org/a#"))(
          PrefixMap.addPrefix("", IRI("http://example.org#"))(
            PrefixMap.addPrefix("año", IRI("http://example.org/año#"))(
              PrefixMap.empty
            )
          )
        )
      val p = PrefixedName(prefixMap)
      shouldParseGeneric(p, ":a", IRI("http://example.org#a"))
      shouldParseGeneric(p, "a:b", IRI("http://example.org/a#b"))
      shouldParseGeneric(p, "a:", IRI("http://example.org/a#"))
      shouldNotParse(p, "<a>")
    }

    describe("BlankNode") {
      val initial = BNodeTable.empty
      val p = BlankNode _
      shouldParseRDF(p(initial), "_:1", BNodeId("1"))
      shouldParseRDF(p(initial), "_:a", BNodeId("a"))
      println("Initial: " + initial)
      shouldParseRDF(p(initial), "[]", BNodeId("b" + 0))
      //    shouldNotParse(p(initial), "<a>")
    }

    def shouldParseState[A, S](
      p: S => Parser[(A, S)],
      s: String,
      a: A,
      initial: S,
      end: S
    ): Unit = {
      it("Should parse \"" + s + "\"") {
        val result = parseAll(p(initial), s) match {
          case Success((x, s1), _) => {
            x should be(a)
            s1 should be(end)
          }
          case NoSuccess(msg, _) => fail(msg)
        }
      }
    }

    /**
     *  This is the same as shouldParseGeneric but
     *  ignores the state
     */
    def shouldParseRDF[A](p: Parser[A], s: String, a: A) {
      it("Should parse \"" + s + "\"" + " and return " + a.toString) {
        val result = parseAll(p, s) match {
          case Success((x, _), _) => x
          case NoSuccess(msg, _) => fail(msg)
        }
        result should be(a)
      }
    }

    def shouldParseTurtle(
      s: String,
      baseIRI: IRI = IRI(""),
      triples: Set[RDFTriple]
    ) {
      it("Should parse \"" + s + "\"" + " and return " + triples) {
        val result = TurtleParser.parse(s, baseIRI) match {
          case util.Success((x, pm)) => x
          case util.Failure(msg) => fail(msg)
        }
        result should be(triples)
      }
    }

    def shouldNotParseTurtle(s: String) {
      it("Should not parse \"" + s + "\"") {
        TurtleParser.parse(s) match {
          case util.Success(x) => fail("Parsed: " + x + " but it should not parse")
          case util.Failure(msg) => info("Not parsed with message: " + msg)
        }
      }
    }
  }
}