package net.ghoula.sarati.xpath

import munit.FunSuite

import net.ghoula.sarati.ast.xml.*

class XPathEvalTests extends FunSuite {

  private def evalOn(doc: XmlDocument, expr: XPathExpr): Either[XPathError, XPathValue] =
    XPathEval.eval(expr, doc)

  private def nodes(doc: XmlDocument, expr: XPathExpr): List[String] =
    evalOn(doc, expr) match {
      case Right(XPathValue.NodeSet(ns)) =>
        ns.map {
          case e: ElementNode => e.name.localName
          case t: TextNode => t.text
          case a: AttributeNode => a.value
          case c: CommentNode => c.text
          case p: PiNode => p.target
          case _: Document => "/"
        }
      case other => fail(s"expected node-set, got $other")
    }

  private def num(doc: XmlDocument, expr: XPathExpr): Double =
    evalOn(doc, expr) match {
      case Right(XPathValue.Number(n)) => n
      case other => fail(s"expected number, got $other")
    }

  private def str(doc: XmlDocument, expr: XPathExpr): String =
    evalOn(doc, expr) match {
      case Right(XPathValue.Str(s)) => s
      case other => fail(s"expected string, got $other")
    }

  private def bool(doc: XmlDocument, expr: XPathExpr): Boolean =
    evalOn(doc, expr) match {
      case Right(XPathValue.Bool(b)) => b
      case other => fail(s"expected bool, got $other")
    }

  // ==== builders ====

  private def elem(local: String, children: XmlNode*): XmlNode.Element =
    XmlNode.Element(qname(local), List.empty, children.toList)

  private def elem(name: QName, children: XmlNode*): XmlNode.Element =
    XmlNode.Element(name, List.empty, children.toList)

  private def withAttrsAndText(local: String, text: String, attrs: (String, String)*): XmlNode.Element =
    XmlNode.Element(
      qname(local),
      attrs.map((n, v) => (name = qname(n), value = v)).toList,
      List(XmlNode.Text(text))
    )

  private def doc(root: XmlNode): XmlDocument =
    (version = "1.0", encoding = Some("UTF-8"), standalone = None, root = root)

  private def step(axis: Axis, test: NodeTest, preds: XPathExpr*): Step =
    Step(axis, test, preds.toList)

  private def rel(test: NodeTest, preds: XPathExpr*): XPathExpr =
    XPathExpr.Path(isAbsolute = false, List(step(Axis.Child, test, preds*)))

  private def abs(steps: Step*): XPathExpr =
    XPathExpr.Path(isAbsolute = true, steps.toList)

  /** Absolute path doc/r/<test> with optional predicates — the common corpus shape. */
  private def underR(test: NodeTest, preds: XPathExpr*): XPathExpr =
    XPathExpr.Path(
      true,
      List(step(Axis.Child, name("r")), step(Axis.Child, test, preds*))
    )

  private def name(local: String): NodeTest = NodeTest.Name(NameTest.Named(None, local))

  private def call(n: String, args: XPathExpr*): XPathExpr = XPathExpr.FunctionCall(n, args.toList)

  private def lit(s: String): XPathExpr = XPathExpr.Literal(s)
  private def numLit(d: Double): XPathExpr = XPathExpr.Number(d)

  // ==== D2-style: document order, child steps, predicates ====

  private lazy val d2 = doc(
    elem(
      "r",
      elem("a", elem("b", XmlNode.Text("1"))),
      elem("a", elem("b", XmlNode.Text("2"))),
      elem("c", XmlNode.Text("3"))
    )
  )

  test("child steps select elements in document order") {
    assertEquals(nodes(d2, underR(name("a"))), List("a", "a"))
  }

  test("absolute path from the document node") {
    assertEquals(nodes(d2, abs(step(Axis.Child, name("r")), step(Axis.Child, name("c")))), List("c"))
  }

  test("rooted path alone selects the document node") {
    assertEquals(nodes(d2, abs()), List("/"))
  }

  test("count and positional predicates") {
    assertEquals(num(d2, call("count", underR(name("a")))), 2.0)
    assertEquals(nodes(d2, underR(name("a"), call("last"))), List("a"))
    assertEquals(
      nodes(d2, underR(name("a"), XPathExpr.Comparison(call("position"), numLit(1), BinaryOp.Gt))),
      List("a")
    )
  }

  test("sum over descendant axis") {
    val expr = XPathExpr.Path(
      false,
      List(step(Axis.Child, NodeTest.Name(NameTest.Any)), step(Axis.Descendant, name("b")))
    )
    assertEquals(num(d2, call("sum", expr)), 3.0)
  }

  // ==== D3-style: attribute nodes ====

  private lazy val d3 = doc(
    elem(
      "r",
      withAttrsAndText("item", "v1", "id" -> "7", "kind" -> "x"),
      withAttrsAndText("item", "v2", "id" -> "8")
    )
  )

  test("attribute axis selects attribute values in document order") {
    val expr = XPathExpr.Path(
      true,
      List(
        step(Axis.Child, name("r")),
        step(Axis.Child, name("item")),
        step(Axis.Attribute, NodeTest.Name(NameTest.Named(None, "id")))
      )
    )
    assertEquals(nodes(d3, expr), List("7", "8"))
  }

  test("attribute predicate filters items by id") {
    val expr = XPathExpr.Path(
      true,
      List(
        step(Axis.Child, name("r")),
        step(
          Axis.Child,
          name("item"),
          XPathExpr.Comparison(
            XPathExpr.Path(
              false,
              List(step(Axis.Attribute, NodeTest.Name(NameTest.Named(None, "id"))))
            ),
            numLit(8),
            BinaryOp.Eq
          )
        )
      )
    )
    assertEquals(nodes(d3, expr), List("item"))
  }

  test("name() and local-name() report element names") {
    assertEquals(str(d3, call("name", underR(name("item")))), "item")
    assertEquals(str(d3, call("local-name", underR(name("item")))), "item")
  }

  // ==== D1-style: whitespace text nodes (the xpathXmlConfig shape) ====

  private lazy val d1 = doc(
    elem(
      "a",
      XmlNode.Text(" "),
      elem("b", XmlNode.Text("x1")),
      XmlNode.Text(" tail "),
      elem("b", XmlNode.Text("x2")),
      XmlNode.Text(" ")
    )
  )

  test("whitespace text nodes participate in text()") {
    val texts = XPathExpr.Path(true, List(step(Axis.Child, name("a")), step(Axis.Child, NodeTest.Text)))
    assertEquals(nodes(d1, texts).length, 3)
  }

  test("string-value of an element concatenates descendant text") {
    assertEquals(str(d1, call("string", rel(name("a")))), " x1 tail x2 ")
  }

  // ==== D4-style: CDATA is text-valued but excluded from text() ====

  private lazy val d4 = doc(
    elem("r", elem("t", XmlNode.CData("x < y")), elem("e", XmlNode.Text("a&b")))
  )

  test("CDATA contributes to string values") {
    assertEquals(str(d4, call("string", underR(name("t")))), "x < y")
    assertEquals(str(d4, call("string", underR(name("e")))), "a&b")
    assertEquals(str(d4, call("string", abs(step(Axis.Child, name("r"))))), "x < ya&b")
  }

  private lazy val dCdata = doc(
    elem("r", elem("t", XmlNode.CData("cd")), elem("e", XmlNode.Text("plain")))
  )

  // Spark oracle: xpath('<r><t><![CDATA[cd]]></t><e>plain</e></r>', '//text()') = ["plain"]
  // (its DOM keeps CDATA a distinct node kind; sarati's flag matches that behavior)
  test("text() excludes CDATA; string values, node(), and projections include it") {
    assertEquals(
      nodes(dCdata, abs(step(Axis.DescendantOrSelf, NodeTest.Node), step(Axis.Child, NodeTest.Text))),
      List("plain")
    )
    // string(/r) = cdplain on both backends — CDATA content contributes to string values
    assertEquals(str(dCdata, call("string", abs(step(Axis.Child, name("r"))))), "cdplain")
    // node() counts the folded CDATA node: r, t, cdata, e, text
    assertEquals(
      num(
        dCdata,
        call("count", abs(step(Axis.DescendantOrSelf, NodeTest.Node), step(Axis.Child, NodeTest.Node)))
      ),
      5.0
    )
    // node-value projection: elements → null, CDATA and text → content (the §1 table)
    evalOn(dCdata, abs(step(Axis.DescendantOrSelf, NodeTest.Node), step(Axis.Child, NodeTest.Node))) match {
      case Right(v) =>
        assertEquals(XPathEval.toNodeSetValueList(v), Some(List(None, None, Some("cd"), None, Some("plain"))))
      case other => fail(s"expected node-set, got $other")
    }
  }

  // ==== D6-style: prefix-literal name tests ====

  private lazy val d6 = doc(
    XmlNode.Element(
      qname("r"),
      List((name = qname("xmlns:ns"), value = "urn:x")),
      List(elem(qnameWith("ns", "b"), XmlNode.Text("1")), elem("b", XmlNode.Text("2")))
    )
  )

  test("prefixed name tests match literally") {
    assertEquals(nodes(d6, underR(NodeTest.Name(NameTest.Named(Some("ns"), "b")))), List("b"))
    assertEquals(nodes(d6, underR(NodeTest.Name(NameTest.Named(None, "b")))), List("b"))
  }

  test("wildcard name tests") {
    assertEquals(nodes(d6, underR(NodeTest.Name(NameTest.Any))).length, 2)
    assertEquals(nodes(d6, underR(NodeTest.Name(NameTest.AnyLocalInNamespace("ns")))), List("b"))
    assertEquals(nodes(d6, underR(NodeTest.Name(NameTest.AnyNamespace("b")))), List("b", "b"))
  }

  // ==== D8-style: number coercions and operators ====

  private lazy val d8 = doc(
    elem(
      "r",
      elem("v", XmlNode.Text("42")),
      elem("v", XmlNode.Text(" 4.5 ")),
      elem("v", XmlNode.Text("abc")),
      elem("v", XmlNode.Text("1e3"))
    )
  )

  test("number() of non-numeric text is NaN") {
    // number(//v) takes the FIRST node's number — the NaN case is the non-numeric third v
    assertEquals(num(d8, call("number", underR(name("v")))), 42.0)
    val third = XPathExpr.Comparison(call("position"), numLit(3), BinaryOp.Eq)
    val n = num(d8, call("number", underR(name("v"), third)))
    assert(n.isNaN, s"expected NaN, got $n")
  }

  test("sum propagates NaN from non-numeric nodes (XPath 1.0 semantics)") {
    assert(num(d8, call("sum", underR(name("v")))).isNaN)
  }

  test("arithmetic operators") {
    assertEquals(num(d8, XPathExpr.Arithmetic(numLit(1), numLit(2), ArithOp.Add)), 3.0)
    assertEquals(num(d8, XPathExpr.Arithmetic(numLit(7), numLit(2), ArithOp.Div)), 3.5)
    assertEquals(num(d8, XPathExpr.Arithmetic(numLit(7), numLit(2), ArithOp.Mod)), 1.0)
    assertEquals(num(d8, XPathExpr.Negation(numLit(5))), -5.0)
  }

  test("comparison operators with NaN semantics") {
    val eqNaN = XPathExpr.Comparison(numLit(Double.NaN), numLit(Double.NaN), BinaryOp.Eq)
    assertEquals(bool(d8, eqNaN), false)
    val lt = XPathExpr.Comparison(numLit(1), numLit(2), BinaryOp.Lt)
    assertEquals(bool(d8, lt), true)
  }

  test("div by zero yields Infinity") {
    val dv = XPathExpr.Arithmetic(numLit(1), numLit(0), ArithOp.Div)
    assertEquals(num(d8, dv), Double.PositiveInfinity)
  }

  // ==== functions ====

  test("boolean, not, true, false") {
    assertEquals(bool(d2, call("boolean", underR(name("a")))), true)
    assertEquals(bool(d2, call("not", underR(name("a")))), false)
    assertEquals(bool(d2, call("true")), true)
    assertEquals(bool(d2, call("false")), false)
    assertEquals(bool(d2, call("boolean", underR(name("z")))), false)
  }

  test("string functions") {
    assertEquals(str(d2, call("concat", underR(name("c")), lit("!"))), "3!")
    assertEquals(bool(d2, call("contains", underR(name("c")), lit("3"))), true)
    assertEquals(str(d8, call("normalize-space", underR(name("v")))), "42")
    assertEquals(num(d2, call("string-length", lit("abcd"))), 4.0)
    assertEquals(str(d8, call("substring", underR(name("v")), numLit(2))), "2")
  }

  test("floor, ceiling, round with XPath rounding") {
    assertEquals(num(d8, call("floor", numLit(2.7))), 2.0)
    assertEquals(num(d8, call("ceiling", numLit(2.1))), 3.0)
    assertEquals(num(d8, call("round", numLit(2.5))), 3.0)
    assertEquals(num(d8, call("round", numLit(-2.5))), -2.0)
  }

  test("substring-before and substring-after") {
    assertEquals(str(d8, call("substring-before", lit("a-b"), lit("-"))), "a")
    assertEquals(str(d8, call("substring-after", lit("a-b"), lit("-"))), "b")
  }

  test("translate") {
    assertEquals(str(d8, call("translate", lit("bar"), lit("abc"), lit("ABC"))), "BAr")
  }

  // ==== documented divergences ====

  test("id() returns an empty node-set") {
    assertEquals(evalOn(d2, call("id", lit("x"))), Right(XPathValue.NodeSet(List.empty)))
  }

  test("namespace axis returns empty") {
    assertEquals(
      evalOn(d6, XPathExpr.Path(false, List(step(Axis.Namespace, NodeTest.Node)))),
      Right(XPathValue.NodeSet(List.empty))
    )
  }

  test("variable references are unsupported") {
    evalOn(d2, XPathExpr.VariableRef("x")) match {
      case Left(XPathError.Unsupported(_)) => ()
      case other => fail(s"expected Unsupported, got $other")
    }
  }

  // ==== axis coverage: reverse, siblings, self, unions, filters ====

  test("reverse axes: parent, ancestor, preceding-sibling, preceding") {
    val as =
      XPathExpr.Path(true, List(step(Axis.Child, name("r")), step(Axis.Child, name("a")), step(Axis.Child, name("b"))))
    assertEquals(nodes(d2, as), List("b", "b"))
    val item2 = XPathExpr.Path(
      true,
      List(
        step(Axis.Child, name("r")),
        step(Axis.Child, name("a")),
        step(Axis.Child, name("b")),
        step(Axis.Parent, NodeTest.Node)
      )
    )
    assertEquals(nodes(d2, item2).length, 2)
    val ancestors = XPathExpr.Path(
      true,
      List(
        step(Axis.Child, name("r")),
        step(Axis.Child, name("a")),
        step(Axis.Descendant, name("b")),
        step(Axis.Ancestor, NodeTest.Name(NameTest.Any))
      )
    )
    // b(3) ancestors: a, r; b(6) ancestors: a, r — the merged node-set is document-ordered
    assertEquals(nodes(d2, ancestors), List("r", "a", "a"))
    val precedingSiblings = XPathExpr.Path(
      true,
      List(
        step(Axis.Child, name("r")),
        step(Axis.Child, name("c")),
        step(Axis.PrecedingSibling, name("a"))
      )
    )
    assertEquals(nodes(d2, precedingSiblings), List("a", "a"))
    val preceding = XPathExpr.Path(
      true,
      List(
        step(Axis.Child, name("r")),
        step(Axis.Child, name("c")),
        step(Axis.Preceding, NodeTest.Name(NameTest.Any))
      )
    )
    assertEquals(nodes(d2, preceding).length, 4)
  }

  test("self, descendant-or-self, ancestor-or-self, following") {
    val selfNodes = XPathExpr.Path(true, List(step(Axis.Child, name("r")), step(Axis.Self, NodeTest.Node)))
    assertEquals(nodes(d2, selfNodes), List("r"))
    val dos = XPathExpr.Path(
      true,
      List(step(Axis.Child, name("r")), step(Axis.Child, name("a")), step(Axis.DescendantOrSelf, NodeTest.Node))
    )
    assertEquals(nodes(d2, dos).length, 6)
    val aos = XPathExpr.Path(
      true,
      List(step(Axis.Child, name("r")), step(Axis.Child, name("a")), step(Axis.AncestorOrSelf, NodeTest.Node))
    )
    // each a contributes [a, r, document]; merged distinct, document-ordered = /, r, a, a = 4
    assertEquals(nodes(d2, aos), List("/", "r", "a", "a"))
    val following = XPathExpr.Path(
      true,
      List(
        step(Axis.Child, name("r")),
        step(Axis.Child, name("a")),
        step(Axis.Child, name("b")),
        step(Axis.Following, NodeTest.Node)
      )
    )
    // following(b3) = a, b, text(2), c, text(3); following(b6) = c, text(3) - merged doc order = 5
    assertEquals(nodes(d2, following), List("a", "b", "2", "c", "3"))
  }

  test("union, filter paths, negation") {
    val union = XPathExpr.Union(underR(name("a")), underR(name("c")))
    assertEquals(nodes(d2, union), List("a", "a", "c"))
    val neg = XPathExpr.Negation(numLit(3))
    assertEquals(num(d2, neg), -3.0)
    val filtered = XPathExpr.Filter(
      XPathExpr.Path(true, List(step(Axis.Child, name("r")), step(Axis.Child, name("a")))),
      List.empty,
      List(step(Axis.Child, name("b")))
    )
    assertEquals(nodes(d2, filtered), List("b", "b"))
  }

  test("filter predicates run over the primary's node-set in document order") {
    // (r/a)[2]: the position counts over the primary's whole result, not per-node axis results
    assertEquals(
      nodes(d2, XPathExpr.Filter(underR(name("a")), List(numLit(2)), List.empty)),
      List("a")
    )
    // (r/a)[last()]
    assertEquals(
      nodes(d2, XPathExpr.Filter(underR(name("a")), List(call("last")), List.empty)),
      List("a")
    )
    // boolean-shaped predicate keeps the survivors of the primary
    assertEquals(
      nodes(d2, XPathExpr.Filter(underR(name("a")), List(rel(name("b"))), List.empty)),
      List("a", "a")
    )
  }

  test("filter predicates keep document order across nesting levels") {
    // <r><a><a><a>t</a></a></a><a>u</a></r>: the primary //a yields the outer a, the middle a,
    // then the sibling a; position 2 must pick the middle a, not the first a of any per-node walk
    val nested = doc(
      elem("r", elem("a", elem("a", elem("a", XmlNode.Text("t")))), elem("a", XmlNode.Text("u")))
    )
    val primary = XPathExpr.Path(
      true,
      List(step(Axis.DescendantOrSelf, NodeTest.Node), step(Axis.Child, name("a")))
    )
    assertEquals(
      nodes(nested, XPathExpr.Filter(primary, List(numLit(2)), List.empty)),
      List("a")
    )
    // (//a)[2]/a: steps run against the survivor — the middle a's child a
    assertEquals(
      nodes(nested, XPathExpr.Filter(primary, List(numLit(2)), List(step(Axis.Child, name("a"))))),
      List("a")
    )
  }

  test("filter predicates on a non-node-set primary fail cleanly") {
    evalOn(d2, XPathExpr.Filter(numLit(1), List(numLit(1)), List.empty)) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
  }

  test("unsupported functions and invalid arity fail cleanly") {
    evalOn(d2, call("unknown-fn")) match {
      case Left(XPathError.Unsupported(_)) => ()
      case other => fail(s"expected Unsupported, got $other")
    }
    evalOn(d2, call("count")) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
    evalOn(d2, call("concat", lit("a"))) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
  }

  test("no-arg string/number forms use the context node") {
    // evaluate with an `a` element as the context node (via a filter anchor)
    val aPath = XPathExpr.Path(true, List(step(Axis.Child, name("r")), step(Axis.Child, name("a"))))
    val aNodes = evalOn(d2, aPath) match {
      case Right(XPathValue.NodeSet(ns)) => ns
      case other => fail(s"expected node-set, got $other")
    }
    val ctx = Context(aNodes.head, 1, 1)
    XPathEval.eval(call("string"), ctx) match {
      case Right(XPathValue.Str(s)) => assertEquals(s, "1")
      case other => fail(s"expected string, got $other")
    }
    XPathEval.eval(call("number"), ctx) match {
      case Right(XPathValue.Number(n)) => assertEquals(n, 1.0)
      case other => fail(s"expected number, got $other")
    }
  }

  test("lang checks the nearest xml:lang") {
    val langDoc = doc(
      XmlNode.Element(
        qname("r"),
        List((name = qname("xml:lang"), value = "en")),
        List(elem("p", XmlNode.Text("text")))
      )
    )
    val pPath = XPathExpr.Path(true, List(step(Axis.Child, name("r")), step(Axis.Child, name("p"))))
    val pNode = evalOn(langDoc, pPath) match {
      case Right(XPathValue.NodeSet(n :: _)) => n
      case other => fail(s"expected p node, got $other")
    }
    val ctx = Context(pNode, 1, 1)
    XPathEval.eval(call("lang", lit("en")), ctx) match {
      case Right(XPathValue.Bool(b)) => assert(b, "lang en should match")
      case other => fail(s"expected bool, got $other")
    }
    XPathEval.eval(call("lang", lit("fr")), ctx) match {
      case Right(XPathValue.Bool(b)) => assert(!b, "lang fr should not match")
      case other => fail(s"expected bool, got $other")
    }
  }

  // ==== projections ====

  test("toNodeSetValueList maps elements to None") {
    evalOn(d2, underR(name("a"))) match {
      case Right(v) =>
        assertEquals(XPathEval.toNodeSetValueList(v), Some(List(None, None)))
      case other => fail(s"expected node-set, got $other")
    }
  }

  test("truncateToInt applies the NaN → 0 rule") {
    assertEquals(XPathEval.truncateToInt(Double.NaN), 0)
    assertEquals(XPathEval.truncateToInt(4.9), 4)
    assertEquals(XPathEval.truncateToLong(Double.NaN), 0L)
  }

  // ==== deep nesting: the wrapper must not overflow ====

  test("500-level nested document wraps and evaluates") {
    val depth = 500
    val nested = (1 to depth).foldRight[XmlNode](elem("leaf", XmlNode.Text("1")))((_, acc) => elem("n", acc))
    val deep = doc(nested)
    XPathEval.eval(call("count", abs(step(Axis.Descendant, name("n")))), deep) match {
      case Right(XPathValue.Number(n)) => assertEquals(n, depth.toDouble)
      case other => fail(s"expected count, got $other")
    }
  }
  // ==== branch-coverage round: error arms, coercions, edge axes ====

  test("union of non-node-sets fails") {
    evalOn(d2, XPathExpr.Union(numLit(1), numLit(2))) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
  }

  test("location steps on a non-node-set primary fail") {
    val badFilter = XPathExpr.Filter(numLit(1), List.empty, List(step(Axis.Child, name("x"))))
    evalOn(d2, badFilter) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
  }

  test("a failing predicate fails the step") {
    val badPred = XPathExpr.Path(
      true,
      List(step(Axis.Child, name("r")), step(Axis.Child, name("a"), call("unknown-fn")))
    )
    evalOn(d2, badPred) match {
      case Left(XPathError.Unsupported(_)) => ()
      case other => fail(s"expected Unsupported, got $other")
    }
  }

  test("number coercion of booleans and strings") {
    assertEquals(num(d2, call("number", call("true"))), 1.0)
    assertEquals(num(d2, call("number", call("false"))), 0.0)
    val abcNum = num(d2, call("number", lit("abc")))
    assert(abcNum.isNaN, s"expected NaN, got $abcNum")
  }

  test("string coercion of booleans, numbers, node-sets") {
    assertEquals(str(d2, call("string", call("true"))), "true")
    assertEquals(str(d2, call("string", numLit(7))), "7")
    assertEquals(str(d2, call("string", rel(name("z")))), "")
  }

  test("formatNumber edge values") {
    assertEquals(XPathEval.formatNumber(Double.PositiveInfinity), "Infinity")
    assertEquals(XPathEval.formatNumber(Double.NegativeInfinity), "-Infinity")
    assertEquals(XPathEval.formatNumber(Double.NaN), "NaN")
  }

  test("function arity and node-set guards") {
    evalOn(d2, call("count", lit("x"))) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
    evalOn(d2, call("sum", lit("x"))) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
    evalOn(d2, call("last", numLit(1))) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
    evalOn(d2, call("position", numLit(1))) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
    evalOn(d2, call("true", numLit(1))) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
    evalOn(d2, call("false", numLit(1))) match {
      case Left(XPathError.Invalid(_)) => ()
      case other => fail(s"expected Invalid, got $other")
    }
  }

  test("string()/number() of a non-node-set argument") {
    // names exist only on nodes: non-node arguments yield "" (an X0-corpus item pins the
    // Spark-oracle behavior for these forms)
    assertEquals(str(d2, call("local-name", lit("x"))), "")
    assertEquals(str(d2, call("namespace-uri", lit("x"))), "")
  }

  test("lang subtag matching") {
    val langDoc = doc(
      XmlNode.Element(
        qname("r"),
        List((name = qname("xml:lang"), value = "en-US")),
        List(elem("p", XmlNode.Text("text")))
      )
    )
    val pPath = XPathExpr.Path(true, List(step(Axis.Child, name("r")), step(Axis.Child, name("p"))))
    val pNode = evalOn(langDoc, pPath) match {
      case Right(XPathValue.NodeSet(n :: _)) => n
      case other => fail(s"expected p node, got $other")
    }
    val ctx = Context(pNode, 1, 1)
    XPathEval.eval(call("lang", lit("en")), ctx) match {
      case Right(XPathValue.Bool(b)) => assert(b, "en matches en-US by subtag")
      case other => fail(s"expected bool, got $other")
    }
  }

  test("name tests do not cross prefixes") {
    assertEquals(
      nodes(d6, underR(NodeTest.Name(NameTest.Named(Some("other"), "b")))),
      List.empty
    )
  }

  test("empty node-set coercions") {
    assertEquals(bool(d2, call("boolean", rel(name("z")))), false)
    assertEquals(str(d2, call("string", rel(name("z")))), "")
    val emptyNum = num(d2, call("number", rel(name("z"))))
    assert(emptyNum.isNaN, s"expected NaN for empty node-set, got $emptyNum")
  }
}
