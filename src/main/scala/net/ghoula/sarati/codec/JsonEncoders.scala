package net.ghoula.sarati.codec

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime}
import java.util.UUID

import net.ghoula.sarati.ast.json.JsonValue

/** [[Encoder]] instances to [[JsonValue]] from primitives, `java.time` types, `UUID`, and the
  * common collection shapes.
  *
  * All numeric types encode through `Double` (`JsonValue.Number` has no integer variant), so
  * `Long`/`BigInt`/`BigDecimal` values beyond the exact-`Double` range (2^53) lose precision.
  * Date-time and UUID values encode as their `toString` forms; `Option` maps `None` to `Null`.
  */
object JsonEncoders {

  given Encoder[String, JsonValue] = new Encoder[String, JsonValue] {
    def encode(value: String): JsonValue = JsonValue.Str(value)
  }

  given Encoder[Int, JsonValue] = new Encoder[Int, JsonValue] {
    def encode(value: Int): JsonValue = JsonValue.Number(value.toDouble)
  }

  given Encoder[Long, JsonValue] = new Encoder[Long, JsonValue] {
    def encode(value: Long): JsonValue = JsonValue.Number(value.toDouble)
  }

  given Encoder[Double, JsonValue] = new Encoder[Double, JsonValue] {
    def encode(value: Double): JsonValue = JsonValue.Number(value)
  }

  given Encoder[Boolean, JsonValue] = new Encoder[Boolean, JsonValue] {
    def encode(value: Boolean): JsonValue = JsonValue.Bool(value)
  }

  given Encoder[Byte, JsonValue] = new Encoder[Byte, JsonValue] {
    def encode(value: Byte): JsonValue = JsonValue.Number(value.toDouble)
  }

  given Encoder[Short, JsonValue] = new Encoder[Short, JsonValue] {
    def encode(value: Short): JsonValue = JsonValue.Number(value.toDouble)
  }

  given Encoder[Float, JsonValue] = new Encoder[Float, JsonValue] {
    def encode(value: Float): JsonValue = JsonValue.Number(value.toDouble)
  }

  given Encoder[BigInt, JsonValue] = new Encoder[BigInt, JsonValue] {
    def encode(value: BigInt): JsonValue = JsonValue.Number(value.toDouble)
  }

  given Encoder[BigDecimal, JsonValue] = new Encoder[BigDecimal, JsonValue] {
    def encode(value: BigDecimal): JsonValue = JsonValue.Number(value.toDouble)
  }

  given Encoder[Instant, JsonValue] = new Encoder[Instant, JsonValue] {
    def encode(value: Instant): JsonValue = JsonValue.Str(value.toString)
  }

  given Encoder[LocalDate, JsonValue] = new Encoder[LocalDate, JsonValue] {
    def encode(value: LocalDate): JsonValue = JsonValue.Str(value.toString)
  }

  given Encoder[LocalDateTime, JsonValue] = new Encoder[LocalDateTime, JsonValue] {
    def encode(value: LocalDateTime): JsonValue = JsonValue.Str(value.toString)
  }

  given Encoder[LocalTime, JsonValue] = new Encoder[LocalTime, JsonValue] {
    def encode(value: LocalTime): JsonValue = JsonValue.Str(value.toString)
  }

  given Encoder[OffsetDateTime, JsonValue] = new Encoder[OffsetDateTime, JsonValue] {
    def encode(value: OffsetDateTime): JsonValue = JsonValue.Str(value.toString)
  }

  given Encoder[ZonedDateTime, JsonValue] = new Encoder[ZonedDateTime, JsonValue] {
    def encode(value: ZonedDateTime): JsonValue = JsonValue.Str(value.toString)
  }

  given Encoder[UUID, JsonValue] = new Encoder[UUID, JsonValue] {
    def encode(value: UUID): JsonValue = JsonValue.Str(value.toString)
  }

  given [A] => (encoder: Encoder[A, JsonValue]) => Encoder[Option[A], JsonValue] =
    new Encoder[Option[A], JsonValue] {
      def encode(value: Option[A]): JsonValue = encodeEval(value).run()

      override def encodeEval(value: Option[A]): Eval[JsonValue] =
        Eval.defer {
          value match {
            case None => Eval.now(JsonValue.Null)
            case Some(a) => encoder.encodeEval(a)
          }
        }
    }

  given [A] => (encoder: Encoder[A, JsonValue]) => Encoder[List[A], JsonValue] =
    new Encoder[List[A], JsonValue] {
      def encode(value: List[A]): JsonValue = encodeEval(value).run()

      override def encodeEval(value: List[A]): Eval[JsonValue] =
        Encoder.drainElements[A, JsonValue, JsonValue](value, encoder.encodeEval, finish = JsonValue.Array.apply)
    }

  given [A] => (encoder: Encoder[A, JsonValue]) => Encoder[Seq[A], JsonValue] =
    new Encoder[Seq[A], JsonValue] {
      def encode(value: Seq[A]): JsonValue = encodeEval(value).run()

      override def encodeEval(value: Seq[A]): Eval[JsonValue] =
        Encoder.drainElements[A, JsonValue, JsonValue](value.toList, encoder.encodeEval, finish = JsonValue.Array.apply)
    }

  given [A] => (encoder: Encoder[A, JsonValue]) => Encoder[Vector[A], JsonValue] =
    new Encoder[Vector[A], JsonValue] {
      def encode(value: Vector[A]): JsonValue = encodeEval(value).run()

      override def encodeEval(value: Vector[A]): Eval[JsonValue] =
        Encoder.drainElements[A, JsonValue, JsonValue](value.toList, encoder.encodeEval, finish = JsonValue.Array.apply)
    }

  given [A] => (encoder: Encoder[A, JsonValue]) => Encoder[Map[String, A], JsonValue] =
    new Encoder[Map[String, A], JsonValue] {
      def encode(value: Map[String, A]): JsonValue = encodeEval(value).run()

      override def encodeEval(value: Map[String, A]): Eval[JsonValue] =
        Encoder.drainElements[(String, A), (String, JsonValue), JsonValue](
          value.toList,
          encodeElem = { case (key, a) => encoder.encodeEval(a).map(fv => key -> fv) },
          finish = entries => JsonValue.Object(entries.toMap)
        )
    }
}
