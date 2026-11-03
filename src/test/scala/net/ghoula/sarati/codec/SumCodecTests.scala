package net.ghoula.sarati.codec

import munit.FunSuite

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.json.JsonValue

enum TestStatus derives CanEqual {
  case Active, Inactive
}

case class Account(id: String, status: TestStatus) derives CanEqual

enum Shape derives CanEqual {
  case Circle(radius: Double)
  case Rectangle(width: Double, height: Double)
}

sealed trait Payment derives CanEqual
case class Card(number: String) extends Payment derives CanEqual
case class Cash(amount: Double) extends Payment derives CanEqual

enum Command derives CanEqual {
  case Ping
  case Echo(message: String)
}

class SumCodecTests extends FunSuite {

  test("Simple parameterless enum: encode and decode via JsonEncoders/JsonDecoders") {
    import JsonEncoders.given
    import JsonDecoders.given

    val activeJson = Encoder[TestStatus, JsonValue].encode(TestStatus.Active)
    assertEquals(activeJson, JsonValue.Str("Active"))

    val inactiveJson = Encoder[TestStatus, JsonValue].encode(TestStatus.Inactive)
    assertEquals(inactiveJson, JsonValue.Str("Inactive"))

    val decodedActive = Decoder[JsonValue, TestStatus].decode(JsonValue.Str("Active"))
    assertEquals(decodedActive, Result.Success(TestStatus.Active, 0))

    val decodedInactive = Decoder[JsonValue, TestStatus].decode(JsonValue.Str("Inactive"))
    assertEquals(decodedInactive, Result.Success(TestStatus.Inactive, 0))
  }

  test("Simple parameterless enum: decode errors on unknown label or invalid type") {
    import JsonDecoders.given

    val unknownResult = Decoder[JsonValue, TestStatus].decode(JsonValue.Str("Pending"))
    assert(unknownResult.isFailure)

    val wrongTypeResult = Decoder[JsonValue, TestStatus].decode(JsonValue.Number(42.0))
    assert(wrongTypeResult.isFailure)
  }

  test("Case class with enum field: derived Encoder and Decoder") {
    import JsonEncoders.given
    import JsonDecoders.given

    given Encoder[Account, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Account] = Decoder.derived

    val acc = Account("acc-1", TestStatus.Active)
    val json = Encoder[Account, JsonValue].encode(acc)
    assertEquals(
      json,
      JsonValue.Object(
        Map(
          "id" -> JsonValue.Str("acc-1"),
          "status" -> JsonValue.Str("Active")
        )
      )
    )

    val decoded = Decoder[JsonValue, Account].decode(json)
    assertEquals(decoded, Result.Success(acc, 0))
  }

  test("Java enum: encode and decode") {
    import JsonEncoders.given
    import JsonDecoders.given

    given scala.reflect.ClassTag[java.lang.Thread.State] = scala.reflect.ClassTag(classOf[java.lang.Thread.State])

    val state = java.lang.Thread.State.RUNNABLE
    val json = Encoder[java.lang.Thread.State, JsonValue].encode(state)
    assertEquals(json, JsonValue.Str("RUNNABLE"))

    val decoded = Decoder[JsonValue, java.lang.Thread.State].decode(json)
    assertEquals(decoded, Result.Success(java.lang.Thread.State.RUNNABLE, 0))

    val invalid = Decoder[JsonValue, java.lang.Thread.State].decode(JsonValue.Str("INVALID_STATE"))
    assert(invalid.isFailure)
  }

  test("ADT enum: encode with _type and case fields") {
    import JsonEncoders.given

    given Encoder[Shape, JsonValue] = Encoder.derived

    val circle: Shape = Shape.Circle(4.5)
    val circleJson = Encoder[Shape, JsonValue].encode(circle)
    assertEquals(
      circleJson,
      JsonValue.Object(
        Map(
          "_type" -> JsonValue.Str("Circle"),
          "radius" -> JsonValue.Number(4.5)
        )
      )
    )

    val rect: Shape = Shape.Rectangle(3.0, 4.0)
    val rectJson = Encoder[Shape, JsonValue].encode(rect)
    assertEquals(
      rectJson,
      JsonValue.Object(
        Map(
          "_type" -> JsonValue.Str("Rectangle"),
          "width" -> JsonValue.Number(3.0),
          "height" -> JsonValue.Number(4.0)
        )
      )
    )
  }

  test("ADT enum: decode by _type tag") {
    import JsonDecoders.given

    given Decoder[JsonValue, Shape] = Decoder.derived

    val circleJson = JsonValue.Object(
      Map(
        "_type" -> JsonValue.Str("Circle"),
        "radius" -> JsonValue.Number(4.5)
      )
    )
    val decodedCircle = Decoder[JsonValue, Shape].decode(circleJson)
    assertEquals(decodedCircle, Result.Success(Shape.Circle(4.5), 0))

    val rectJson = JsonValue.Object(
      Map(
        "_type" -> JsonValue.Str("Rectangle"),
        "width" -> JsonValue.Number(3.0),
        "height" -> JsonValue.Number(4.0)
      )
    )
    val decodedRect = Decoder[JsonValue, Shape].decode(rectJson)
    assertEquals(decodedRect, Result.Success(Shape.Rectangle(3.0, 4.0), 0))
  }

  test("ADT enum: decode fallback to ordinal index") {
    import JsonDecoders.given

    given Decoder[JsonValue, Shape] = Decoder.derived

    val circleByIntType = JsonValue.Object(
      Map(
        "_type" -> JsonValue.Number(0.0),
        "radius" -> JsonValue.Number(4.5)
      )
    )
    val decodedCircleInt = Decoder[JsonValue, Shape].decode(circleByIntType)
    assertEquals(decodedCircleInt, Result.Success(Shape.Circle(4.5), 0))

    val rectByOrdinalField = JsonValue.Object(
      Map(
        "_ordinal" -> JsonValue.Number(1.0),
        "width" -> JsonValue.Number(3.0),
        "height" -> JsonValue.Number(4.0)
      )
    )
    val decodedRectOrd = Decoder[JsonValue, Shape].decode(rectByOrdinalField)
    assertEquals(decodedRectOrd, Result.Success(Shape.Rectangle(3.0, 4.0), 0))
  }

  test("ADT enum: decode error handling") {
    import JsonDecoders.given

    given Decoder[JsonValue, Shape] = Decoder.derived

    // Missing _type and _ordinal
    val missingType = JsonValue.Object(Map("radius" -> JsonValue.Number(4.5)))
    val resMissing = Decoder[JsonValue, Shape].decode(missingType)
    assert(resMissing.isFailure)

    // Unknown variant type
    val unknownType = JsonValue.Object(
      Map(
        "_type" -> JsonValue.Str("Triangle"),
        "base" -> JsonValue.Number(2.0)
      )
    )
    val resUnknown = Decoder[JsonValue, Shape].decode(unknownType)
    assert(resUnknown.isFailure)

    // Non-object JSON
    val nonObject = JsonValue.Array(List(JsonValue.Str("Circle")))
    val resNonObj = Decoder[JsonValue, Shape].decode(nonObject)
    assert(resNonObj.isFailure)

    // Field error within matched variant
    val badField = JsonValue.Object(
      Map(
        "_type" -> JsonValue.Str("Circle"),
        "radius" -> JsonValue.Str("not-a-number")
      )
    )
    val resBadField = Decoder[JsonValue, Shape].decode(badField)
    assert(resBadField.isFailure)
  }

  test("Sealed trait hierarchy: encode and decode round trip") {
    import JsonEncoders.given
    import JsonDecoders.given

    given Encoder[Payment, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Payment] = Decoder.derived

    val card: Payment = Card("1234-5678")
    val cardJson = Encoder[Payment, JsonValue].encode(card)
    assertEquals(
      cardJson,
      JsonValue.Object(
        Map(
          "_type" -> JsonValue.Str("Card"),
          "number" -> JsonValue.Str("1234-5678")
        )
      )
    )
    assertEquals(Decoder[JsonValue, Payment].decode(cardJson), Result.Success(card, 0))

    val cash: Payment = Cash(50.0)
    val cashJson = Encoder[Payment, JsonValue].encode(cash)
    assertEquals(
      cashJson,
      JsonValue.Object(
        Map(
          "_type" -> JsonValue.Str("Cash"),
          "amount" -> JsonValue.Number(50.0)
        )
      )
    )
    assertEquals(Decoder[JsonValue, Payment].decode(cashJson), Result.Success(cash, 0))
  }

  test("Mixed ADT (parameterless case and product case)") {
    import JsonEncoders.given
    import JsonDecoders.given

    given Encoder[Command, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Command] = Decoder.derived

    val ping: Command = Command.Ping
    val pingJson = Encoder[Command, JsonValue].encode(ping)
    assertEquals(pingJson, JsonValue.Object(Map("_type" -> JsonValue.Str("Ping"))))
    assertEquals(Decoder[JsonValue, Command].decode(pingJson), Result.Success(ping, 0))

    val echo: Command = Command.Echo("hello")
    val echoJson = Encoder[Command, JsonValue].encode(echo)
    assertEquals(
      echoJson,
      JsonValue.Object(
        Map(
          "_type" -> JsonValue.Str("Echo"),
          "message" -> JsonValue.Str("hello")
        )
      )
    )
    assertEquals(Decoder[JsonValue, Command].decode(echoJson), Result.Success(echo, 0))
  }

  test("ADT enum: custom discriminator property via SumDiscriminator given") {
    given SumDiscriminator = new SumDiscriminator {
      def propertyName: String = "kind"
    }

    import JsonEncoders.given
    import JsonDecoders.given

    given Encoder[Shape, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Shape] = Decoder.derived

    val circle: Shape = Shape.Circle(2.5)
    val json = Encoder[Shape, JsonValue].encode(circle)
    assertEquals(
      json,
      JsonValue.Object(
        Map(
          "kind" -> JsonValue.Str("Circle"),
          "radius" -> JsonValue.Number(2.5)
        )
      )
    )

    val decoded = Decoder[JsonValue, Shape].decode(json)
    assertEquals(decoded, Result.Success(circle, 0))

    // The custom property replaces "_type" entirely: a payload tagged with "_type" no longer
    // decodes, and a payload missing "kind" reports MissingField("kind").
    val wrongTag = JsonValue.Object(
      Map(
        "_type" -> JsonValue.Str("Circle"),
        "radius" -> JsonValue.Number(2.5)
      )
    )
    assert(Decoder[JsonValue, Shape].decode(wrongTag).isFailure)

    val missingTag = JsonValue.Object(Map("radius" -> JsonValue.Number(2.5)))
    val missingRes = Decoder[JsonValue, Shape].decode(missingTag)
    assert(missingRes.isFailure)
    missingRes match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.MissingField(name, _) => name == "kind"
          case _ => false
        })
      case other => fail(s"Expected Failure, got $other")
    }
  }
}
