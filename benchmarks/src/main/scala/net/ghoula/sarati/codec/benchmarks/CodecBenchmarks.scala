package net.ghoula.sarati.codec.benchmarks

import net.ghoula.sarati.ast.json.JsonValue
import net.ghoula.sarati.codec.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

/** JMH benchmarks for the sarati derived codec.
  *
  * Two factors are measured in isolation elsewhere: the traversal mechanism (recursive vs Eval
  * trampoline) and the erasure (Any-based vs typed tuples). This suite captures the absolute
  * numbers: typical payloads, plus the wide and deep axes the stack-safety work introduced.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class CodecBenchmarks {

  import JsonDecoders.given
  import JsonEncoders.given

  // ============================================================================
  // Case classes
  // ============================================================================

  case class Point(x: Int, y: Int)
  case class Person(name: String, age: Int, active: Boolean)
  case class Address(street: String, city: String, zip: String)
  case class User(name: String, email: String, address: Address)
  case class Node(value: Int, next: Option[Node])

  given Decoder[JsonValue, Point] = Decoder.derived
  given Decoder[JsonValue, Person] = Decoder.derived
  given Decoder[JsonValue, Address] = Decoder.derived
  given Decoder[JsonValue, User] = Decoder.derived
  given Decoder[JsonValue, Node] = Decoder.derived

  given Encoder[Point, JsonValue] = Encoder.derived
  given Encoder[Person, JsonValue] = Encoder.derived
  given Encoder[Address, JsonValue] = Encoder.derived
  given Encoder[User, JsonValue] = Encoder.derived
  given Encoder[Node, JsonValue] = Encoder.derived

  // ============================================================================
  // Test data
  // ============================================================================

  private var jsonUser: JsonValue = uninitialized
  private var user: User = uninitialized

  private var deepJson: JsonValue = uninitialized
  private var deepNode: Node = uninitialized

  private var wideJson: JsonValue = uninitialized
  private var wideList: List[Int] = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    user = User("Alice", "alice@example.com", Address("123 Main St", "Springfield", "12345"))
    jsonUser = Encoder[User, JsonValue].encode(user)

    // Deep: 10_000 nesting levels.
    var j: JsonValue = JsonValue.Null
    for i <- 0 until 10000 do j = JsonValue.Object(Map("value" -> JsonValue.Number(i.toDouble), "next" -> j))
    deepJson = j
    var n: Node = Node(0, None)
    for i <- 1 until 10000 do n = Node(i, Some(n))
    deepNode = n

    // Wide: 100_000-element list.
    wideList = List.fill(100000)(1)
    wideJson = JsonValue.Array(wideList.map(JsonValue.Number.apply(_)))
  }

  // ============================================================================
  // Typical payloads
  // ============================================================================

  @Benchmark
  def decodeNestedUser(bh: Blackhole): Unit =
    bh.consume(Decoder[JsonValue, User].decode(jsonUser))

  @Benchmark
  def encodeNestedUser(bh: Blackhole): Unit =
    bh.consume(Encoder[User, JsonValue].encode(user))

  // ============================================================================
  // Deep axis (stack safety): 10k nesting
  // ============================================================================

  @Benchmark
  def decodeDeep(bh: Blackhole): Unit =
    bh.consume(Decoder[JsonValue, Node].decode(deepJson))

  @Benchmark
  def encodeDeep(bh: Blackhole): Unit =
    bh.consume(Encoder[Node, JsonValue].encode(deepNode))

  // ============================================================================
  // Wide axis (stack safety): 100k elements
  // ============================================================================

  @Benchmark
  def decodeWide(bh: Blackhole): Unit =
    bh.consume(Decoder[JsonValue, List[Int]].decode(wideJson))

  @Benchmark
  def encodeWide(bh: Blackhole): Unit =
    bh.consume(Encoder[List[Int], JsonValue].encode(wideList))
}
