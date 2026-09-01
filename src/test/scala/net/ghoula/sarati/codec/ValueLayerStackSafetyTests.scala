package net.ghoula.sarati.codec

import munit.FunSuite

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.json.JsonValue

/** The value layer is stack-safe in both width and depth: the derived codec recurses through an
  * [[Eval]] trampoline, so deeply-nested values are drained on the heap, not the call stack. These
  * tests would overflow a naive recursive codec at a few thousand levels.
  */
class ValueLayerStackSafetyTests extends FunSuite {

  import JsonDecoders.given
  import JsonEncoders.given

  private case class Node(value: Int, next: Option[Node])

  private given Decoder[JsonValue, Node] = Decoder.derived
  private given Encoder[Node, JsonValue] = Encoder.derived

  private def deepJson(depth: Int): JsonValue = {
    var json: JsonValue = JsonValue.Null
    for i <- 0 until depth do json = JsonValue.Object(Map("value" -> JsonValue.Number(i.toDouble), "next" -> json))
    json
  }

  private def deepNode(depth: Int): Node = {
    var node: Node = Node(0, None)
    for i <- 1 until depth do node = Node(i, Some(node))
    node
  }

  test("decode is stack-safe at 50,000 nesting levels") {
    assert(Decoder[JsonValue, Node].decode(deepJson(50000)).isSuccess)
  }

  test("encode is stack-safe at 50,000 nesting levels") {
    Encoder[Node, JsonValue].encode(deepNode(50000)) match {
      case JsonValue.Object(_) => ()
      case other => fail(s"expected a JSON object, got ${other.getClass.getSimpleName}")
    }
  }

  test("round-trip a moderately nested value") {
    val node = deepNode(500)
    val encoded = Encoder[Node, JsonValue].encode(node)
    assertEquals(Decoder[JsonValue, Node].decode(encoded), Result.Success(node, 0))
  }

  test("decode is stack-safe at 100,000-element width") {
    val json = JsonValue.Array(List.fill(100000)(JsonValue.Number(42.0)))
    Decoder[JsonValue, List[Int]].decode(json) match {
      case Result.Success(xs, _) =>
        assertEquals(xs.size, 100000)
        assertEquals(xs.head, 42)
      case other => fail(s"expected Success, got ${other.getClass.getSimpleName}")
    }
  }

  test("encode is stack-safe at 100,000-element width") {
    val list = List.fill(100000)(42)
    Encoder[List[Int], JsonValue].encode(list) match {
      case JsonValue.Array(elements) => assertEquals(elements.size, 100000)
      case other => fail(s"expected an array, got ${other.getClass.getSimpleName}")
    }
  }

  test("round-trip a 100,000-element list") {
    val list = List.fill(100000)(42)
    val encoded = Encoder[List[Int], JsonValue].encode(list)
    assertEquals(Decoder[JsonValue, List[Int]].decode(encoded), Result.Success(list, 0))
  }

  test("decode deep-by-wide (1,000 nodes, each 100 deep)") {
    val json = JsonValue.Array(List.fill(1000)(deepJson(100)))
    Decoder[JsonValue, List[Node]].decode(json) match {
      case Result.Success(nodes, _) => assertEquals(nodes.size, 1000)
      case other => fail(s"expected Success, got ${other.getClass.getSimpleName}")
    }
  }
}
