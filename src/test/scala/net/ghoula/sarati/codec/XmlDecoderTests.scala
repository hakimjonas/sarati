package net.ghoula.sarati.codec

import munit.FunSuite

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.xml.{XmlNode, qname}

class XmlDecoderTests extends FunSuite {

  import XmlDecoders.given

  test("decode XML Text node to String") {
    val xml = XmlNode.Text("hello")
    val result = Decoder[XmlNode, String].decode(xml)
    assertEquals(result, Result.Success("hello", 0))
  }

  test("decode XML CData node to String") {
    val xml = XmlNode.CData("<raw content>")
    val result = Decoder[XmlNode, String].decode(xml)
    assertEquals(result, Result.Success("<raw content>", 0))
  }

  test("decode XML Element text content to String") {
    val xml = XmlNode.Element(
      qname("greeting"),
      List.empty,
      List(XmlNode.Text("hello world"))
    )
    val result = Decoder[XmlNode, String].decode(xml)
    assertEquals(result, Result.Success("hello world", 0))
  }

  test("decode XML Element with multiple text children concatenates content") {
    val xml = XmlNode.Element(
      qname("message"),
      List.empty,
      List(
        XmlNode.Text("hello "),
        XmlNode.Text("world")
      )
    )
    val result = Decoder[XmlNode, String].decode(xml)
    assertEquals(result, Result.Success("hello world", 0))
  }

  test("decode XML Element with mixed Text and CData concatenates content") {
    val xml = XmlNode.Element(
      qname("content"),
      List.empty,
      List(
        XmlNode.Text("prefix: "),
        XmlNode.CData("<code>")
      )
    )
    val result = Decoder[XmlNode, String].decode(xml)
    assertEquals(result, Result.Success("prefix: <code>", 0))
  }

  test("decode empty XML Element to empty String") {
    val xml = XmlNode.Element(
      qname("empty"),
      List.empty,
      List.empty
    )
    val result = Decoder[XmlNode, String].decode(xml)
    assertEquals(result, Result.Success("", 0))
  }

  test("decode XML Text to Int") {
    val xml = XmlNode.Text("42")
    val result = Decoder[XmlNode, Int].decode(xml)
    assertEquals(result, Result.Success(42, 0))
  }

  test("decode XML Text with whitespace to Int") {
    val xml = XmlNode.Text("  123  ")
    val result = Decoder[XmlNode, Int].decode(xml)
    assertEquals(result, Result.Success(123, 0))
  }

  test("decode XML Element text to Int") {
    val xml = XmlNode.Element(
      qname("count"),
      List.empty,
      List(XmlNode.Text("99"))
    )
    val result = Decoder[XmlNode, Int].decode(xml)
    assertEquals(result, Result.Success(99, 0))
  }

  test("decode XML Text to Long") {
    val xml = XmlNode.Text("9876543210")
    val result = Decoder[XmlNode, Long].decode(xml)
    assertEquals(result, Result.Success(9876543210L, 0))
  }

  test("decode XML Text to Double") {
    val xml = XmlNode.Text("3.14159")
    val result = Decoder[XmlNode, Double].decode(xml)
    assertEquals(result, Result.Success(3.14159, 0))
  }

  test("decode XML Text to Float") {
    val xml = XmlNode.Text("1.5")
    val result = Decoder[XmlNode, Float].decode(xml)
    assertEquals(result, Result.Success(1.5f, 0))
  }

  test("decode XML Text 'true' to Boolean") {
    val xml = XmlNode.Text("true")
    val result = Decoder[XmlNode, Boolean].decode(xml)
    assertEquals(result, Result.Success(true, 0))
  }

  test("decode XML Text 'false' to Boolean") {
    val xml = XmlNode.Text("false")
    val result = Decoder[XmlNode, Boolean].decode(xml)
    assertEquals(result, Result.Success(false, 0))
  }

  test("decode XML Text '1' to Boolean true") {
    val xml = XmlNode.Text("1")
    val result = Decoder[XmlNode, Boolean].decode(xml)
    assertEquals(result, Result.Success(true, 0))
  }

  test("decode XML Text '0' to Boolean false") {
    val xml = XmlNode.Text("0")
    val result = Decoder[XmlNode, Boolean].decode(xml)
    assertEquals(result, Result.Success(false, 0))
  }

  test("decode XML Text 'yes' to Boolean true (case-insensitive)") {
    val xml = XmlNode.Text("YES")
    val result = Decoder[XmlNode, Boolean].decode(xml)
    assertEquals(result, Result.Success(true, 0))
  }

  test("decode XML Text 'no' to Boolean false (case-insensitive)") {
    val xml = XmlNode.Text("No")
    val result = Decoder[XmlNode, Boolean].decode(xml)
    assertEquals(result, Result.Success(false, 0))
  }

  test("decode XML Text to Option[String]") {
    val xml = XmlNode.Text("hello")
    val result = Decoder[XmlNode, Option[String]].decode(xml)
    assertEquals(result, Result.Success(Some("hello"), 0))
  }

  test("decode empty XML Element to Option[String] as None") {
    val xml = XmlNode.Element(
      qname("optional"),
      List.empty,
      List.empty
    )
    val result = Decoder[XmlNode, Option[String]].decode(xml)
    assertEquals(result, Result.Success(None, 0))
  }

  test("decode whitespace-only XML Text to Option[String] as None") {
    val xml = XmlNode.Text("   ")
    val result = Decoder[XmlNode, Option[String]].decode(xml)
    assertEquals(result, Result.Success(None, 0))
  }

  test("decode XML Element with content to Option[Int]") {
    val xml = XmlNode.Element(
      qname("value"),
      List.empty,
      List(XmlNode.Text("42"))
    )
    val result = Decoder[XmlNode, Option[Int]].decode(xml)
    assertEquals(result, Result.Success(Some(42), 0))
  }

  test("decode XML Element with child elements to List[String]") {
    val xml = XmlNode.Element(
      qname("items"),
      List.empty,
      List(
        XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("apple"))),
        XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("banana"))),
        XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("cherry")))
      )
    )
    val result = Decoder[XmlNode, List[String]].decode(xml)
    assertEquals(result, Result.Success(List("apple", "banana", "cherry"), 0))
  }

  test("decode XML Element with child elements to List[Int]") {
    val xml = XmlNode.Element(
      qname("numbers"),
      List.empty,
      List(
        XmlNode.Element(qname("num"), List.empty, List(XmlNode.Text("1"))),
        XmlNode.Element(qname("num"), List.empty, List(XmlNode.Text("2"))),
        XmlNode.Element(qname("num"), List.empty, List(XmlNode.Text("3")))
      )
    )
    val result = Decoder[XmlNode, List[Int]].decode(xml)
    assertEquals(result, Result.Success(List(1, 2, 3), 0))
  }

  test("decode empty XML Element to empty List") {
    val xml = XmlNode.Element(
      qname("items"),
      List.empty,
      List.empty
    )
    val result = Decoder[XmlNode, List[String]].decode(xml)
    assertEquals(result, Result.Success(List.empty, 0))
  }

  test("decode XML Element ignores text children when decoding List") {
    val xml = XmlNode.Element(
      qname("items"),
      List.empty,
      List(
        XmlNode.Text("ignored whitespace"),
        XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("value"))),
        XmlNode.Text("more ignored")
      )
    )
    val result = Decoder[XmlNode, List[String]].decode(xml)
    assertEquals(result, Result.Success(List("value"), 0))
  }

  test("decode XML Element to Seq[String]") {
    val xml = XmlNode.Element(
      qname("items"),
      List.empty,
      List(
        XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("a"))),
        XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("b")))
      )
    )
    val result = Decoder[XmlNode, Seq[String]].decode(xml)
    assertEquals(result, Result.Success(Seq("a", "b"), 0))
  }

  test("decode XML Element to Vector[Int]") {
    val xml = XmlNode.Element(
      qname("nums"),
      List.empty,
      List(
        XmlNode.Element(qname("n"), List.empty, List(XmlNode.Text("10"))),
        XmlNode.Element(qname("n"), List.empty, List(XmlNode.Text("20")))
      )
    )
    val result = Decoder[XmlNode, Vector[Int]].decode(xml)
    assertEquals(result, Result.Success(Vector(10, 20), 0))
  }

  test("getAttribute returns attribute value") {
    val xml: XmlNode.Element = XmlNode.Element(
      qname("element"),
      List((name = qname("id"), value = "123")),
      List.empty
    )
    val result = XmlDecoders.getAttribute(xml, "id")
    assertEquals(result, Some("123"))
  }

  test("getAttribute returns None for missing attribute") {
    val xml: XmlNode.Element = XmlNode.Element(
      qname("element"),
      List((name = qname("id"), value = "123")),
      List.empty
    )
    val result = XmlDecoders.getAttribute(xml, "name")
    assertEquals(result, None)
  }

  test("getChild returns first matching child element") {
    val xml: XmlNode.Element = XmlNode.Element(
      qname("parent"),
      List.empty,
      List(
        XmlNode.Element(qname("child"), List.empty, List(XmlNode.Text("first"))),
        XmlNode.Element(qname("child"), List.empty, List(XmlNode.Text("second")))
      )
    )
    val result = XmlDecoders.getChild(xml, "child")
    assert(result.isDefined)
    assertEquals(result.get.name.localName, "child")
  }

  test("getChild returns None for missing child") {
    val xml: XmlNode.Element = XmlNode.Element(
      qname("parent"),
      List.empty,
      List(XmlNode.Element(qname("other"), List.empty, List.empty))
    )
    val result = XmlDecoders.getChild(xml, "child")
    assertEquals(result, None)
  }

  test("getChildren returns all matching child elements") {
    val xml: XmlNode.Element = XmlNode.Element(
      qname("parent"),
      List.empty,
      List(
        XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("1"))),
        XmlNode.Element(qname("other"), List.empty, List.empty),
        XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("2"))),
        XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("3")))
      )
    )
    val result = XmlDecoders.getChildren(xml, "item")
    assertEquals(result.length, 3)
  }

  test("getChildren returns empty list when no matches") {
    val xml: XmlNode.Element = XmlNode.Element(
      qname("parent"),
      List.empty,
      List(XmlNode.Element(qname("other"), List.empty, List.empty))
    )
    val result = XmlDecoders.getChildren(xml, "item")
    assertEquals(result, List.empty)
  }

  test("type mismatch error - Int expected, invalid text") {
    val xml = XmlNode.Text("not a number")
    val result = Decoder[XmlNode, Int].decode(xml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Int", actual, _) => actual.contains("not a valid integer")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - Double expected, invalid text") {
    val xml = XmlNode.Text("not a double")
    val result = Decoder[XmlNode, Double].decode(xml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Double", actual, _) =>
            actual.contains("not a valid double")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - Boolean expected, invalid text") {
    val xml = XmlNode.Text("maybe")
    val result = Decoder[XmlNode, Boolean].decode(xml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Boolean", actual, _) =>
            actual.contains("not a valid boolean")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - String expected, Comment found") {
    val xml = XmlNode.Comment("this is a comment")
    val result = Decoder[XmlNode, String].decode(xml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("String", actual, _) => actual.contains("Comment")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - List expected, Text found") {
    val xml = XmlNode.Text("not an element")
    val result = Decoder[XmlNode, List[String]].decode(xml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Element", actual, _) => actual.contains("Text")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - ProcessingInstruction cannot decode to String") {
    val xml = XmlNode.ProcessingInstruction("xml", "version=\"1.0\"")
    val result = Decoder[XmlNode, String].decode(xml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("String", actual, _) =>
            actual.contains("ProcessingInstruction")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }
}
