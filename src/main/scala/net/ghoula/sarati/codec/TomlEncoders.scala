package net.ghoula.sarati.codec

import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime}
import java.util.UUID

import net.ghoula.sarati.ast.toml.TomlValue

/** [[Encoder]] instances to [[TomlValue]] from primitives, `java.time` types, `UUID`, and the
  * common collection shapes.
  *
  * TOML has no null, so `Option` maps `None` to an empty `String` — the same rule as
  * [[AstBuilder]]. `BigInt`/`BigDecimal` encode through `Long`/`Double` and lose precision beyond
  * those ranges. Collections encode as `Array`, string-keyed maps as `InlineTable`.
  */
object TomlEncoders {

  given Encoder[String, TomlValue] = new Encoder[String, TomlValue] {
    def encode(value: String): TomlValue = TomlValue.String(value)
  }

  given Encoder[Int, TomlValue] = new Encoder[Int, TomlValue] {
    def encode(value: Int): TomlValue = TomlValue.Integer(value.toLong)
  }

  given Encoder[Long, TomlValue] = new Encoder[Long, TomlValue] {
    def encode(value: Long): TomlValue = TomlValue.Integer(value)
  }

  given Encoder[Double, TomlValue] = new Encoder[Double, TomlValue] {
    def encode(value: Double): TomlValue = TomlValue.Float(value)
  }

  given Encoder[Boolean, TomlValue] = new Encoder[Boolean, TomlValue] {
    def encode(value: Boolean): TomlValue = TomlValue.Boolean(value)
  }

  given Encoder[Byte, TomlValue] = new Encoder[Byte, TomlValue] {
    def encode(value: Byte): TomlValue = TomlValue.Integer(value.toLong)
  }

  given Encoder[Short, TomlValue] = new Encoder[Short, TomlValue] {
    def encode(value: Short): TomlValue = TomlValue.Integer(value.toLong)
  }

  given Encoder[Float, TomlValue] = new Encoder[Float, TomlValue] {
    def encode(value: Float): TomlValue = TomlValue.Float(value.toDouble)
  }

  given Encoder[BigInt, TomlValue] = new Encoder[BigInt, TomlValue] {
    def encode(value: BigInt): TomlValue = TomlValue.Integer(value.toLong)
  }

  given Encoder[BigDecimal, TomlValue] = new Encoder[BigDecimal, TomlValue] {
    def encode(value: BigDecimal): TomlValue = TomlValue.Float(value.toDouble)
  }

  given Encoder[OffsetDateTime, TomlValue] = new Encoder[OffsetDateTime, TomlValue] {
    def encode(value: OffsetDateTime): TomlValue = TomlValue.DateTime(value)
  }

  given Encoder[LocalDateTime, TomlValue] = new Encoder[LocalDateTime, TomlValue] {
    def encode(value: LocalDateTime): TomlValue = TomlValue.LocalDateTime(value)
  }

  given Encoder[LocalDate, TomlValue] = new Encoder[LocalDate, TomlValue] {
    def encode(value: LocalDate): TomlValue = TomlValue.LocalDate(value)
  }

  given Encoder[LocalTime, TomlValue] = new Encoder[LocalTime, TomlValue] {
    def encode(value: LocalTime): TomlValue = TomlValue.LocalTime(value)
  }

  given Encoder[UUID, TomlValue] = new Encoder[UUID, TomlValue] {
    def encode(value: UUID): TomlValue = TomlValue.String(value.toString)
  }

  given [A] => (encoder: Encoder[A, TomlValue]) => Encoder[Option[A], TomlValue] =
    new Encoder[Option[A], TomlValue] {
      def encode(value: Option[A]): TomlValue = encodeEval(value).run()

      override def encodeEval(value: Option[A]): Eval[TomlValue] =
        Eval.defer {
          value match {
            case None => Eval.now(TomlValue.String(""))
            case Some(a) => encoder.encodeEval(a)
          }
        }
    }

  given [A] => (encoder: Encoder[A, TomlValue]) => Encoder[List[A], TomlValue] =
    new Encoder[List[A], TomlValue] {
      def encode(value: List[A]): TomlValue = encodeEval(value).run()

      override def encodeEval(value: List[A]): Eval[TomlValue] =
        Encoder.drainElements[A, TomlValue, TomlValue](value, encoder.encodeEval, finish = TomlValue.Array.apply)
    }

  given [A] => (encoder: Encoder[A, TomlValue]) => Encoder[Seq[A], TomlValue] =
    new Encoder[Seq[A], TomlValue] {
      def encode(value: Seq[A]): TomlValue = encodeEval(value).run()

      override def encodeEval(value: Seq[A]): Eval[TomlValue] =
        Encoder.drainElements[A, TomlValue, TomlValue](
          value.toList,
          encoder.encodeEval,
          finish = TomlValue.Array.apply
        )
    }

  given [A] => (encoder: Encoder[A, TomlValue]) => Encoder[Vector[A], TomlValue] =
    new Encoder[Vector[A], TomlValue] {
      def encode(value: Vector[A]): TomlValue = encodeEval(value).run()

      override def encodeEval(value: Vector[A]): Eval[TomlValue] =
        Encoder.drainElements[A, TomlValue, TomlValue](
          value.toList,
          encoder.encodeEval,
          finish = TomlValue.Array.apply
        )
    }

  given [A] => (encoder: Encoder[A, TomlValue]) => Encoder[Map[String, A], TomlValue] =
    new Encoder[Map[String, A], TomlValue] {
      def encode(value: Map[String, A]): TomlValue = encodeEval(value).run()

      override def encodeEval(value: Map[String, A]): Eval[TomlValue] =
        Encoder.drainElements[(String, A), (String, TomlValue), TomlValue](
          value.toList,
          encodeElem = { case (key, a) => encoder.encodeEval(a).map(fv => key -> fv) },
          finish = entries => TomlValue.InlineTable(entries.toMap)
        )
    }
}
