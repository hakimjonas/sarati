package net.ghoula.sarati.codec

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime}
import java.util.UUID

import net.ghoula.sarati.ast.yaml.YamlValue

/** [[Encoder]] instances to [[YamlValue]] from primitives, `java.time` types, `UUID`, and the
  * common collection shapes.
  *
  * `YamlValue` has no datetime variant, so the `java.time` types and `UUID` encode through their
  * `toString` forms as `String` (the JSON layer's rule). All numeric types encode through
  * `Long`/`Double` (`Integer`/`Float`), so `BigInt`/`BigDecimal` beyond those ranges lose
  * precision. `Option` maps `None` to `Null`; collections encode as `Sequence`, string-keyed maps
  * as `Mapping`.
  */
object YamlEncoders {

  given Encoder[String, YamlValue] = new Encoder[String, YamlValue] {
    def encode(value: String): YamlValue = YamlValue.String(value)
  }

  given Encoder[Int, YamlValue] = new Encoder[Int, YamlValue] {
    def encode(value: Int): YamlValue = YamlValue.Integer(value.toLong)
  }

  given Encoder[Long, YamlValue] = new Encoder[Long, YamlValue] {
    def encode(value: Long): YamlValue = YamlValue.Integer(value)
  }

  given Encoder[Double, YamlValue] = new Encoder[Double, YamlValue] {
    def encode(value: Double): YamlValue = YamlValue.Float(value)
  }

  given Encoder[Boolean, YamlValue] = new Encoder[Boolean, YamlValue] {
    def encode(value: Boolean): YamlValue = YamlValue.Boolean(value)
  }

  given Encoder[Byte, YamlValue] = new Encoder[Byte, YamlValue] {
    def encode(value: Byte): YamlValue = YamlValue.Integer(value.toLong)
  }

  given Encoder[Short, YamlValue] = new Encoder[Short, YamlValue] {
    def encode(value: Short): YamlValue = YamlValue.Integer(value.toLong)
  }

  given Encoder[Float, YamlValue] = new Encoder[Float, YamlValue] {
    def encode(value: Float): YamlValue = YamlValue.Float(value.toDouble)
  }

  given Encoder[BigInt, YamlValue] = new Encoder[BigInt, YamlValue] {
    def encode(value: BigInt): YamlValue = YamlValue.Integer(value.toLong)
  }

  given Encoder[BigDecimal, YamlValue] = new Encoder[BigDecimal, YamlValue] {
    def encode(value: BigDecimal): YamlValue = YamlValue.Float(value.toDouble)
  }

  given Encoder[Instant, YamlValue] = new Encoder[Instant, YamlValue] {
    def encode(value: Instant): YamlValue = YamlValue.String(value.toString)
  }

  given Encoder[LocalDate, YamlValue] = new Encoder[LocalDate, YamlValue] {
    def encode(value: LocalDate): YamlValue = YamlValue.String(value.toString)
  }

  given Encoder[LocalDateTime, YamlValue] = new Encoder[LocalDateTime, YamlValue] {
    def encode(value: LocalDateTime): YamlValue = YamlValue.String(value.toString)
  }

  given Encoder[LocalTime, YamlValue] = new Encoder[LocalTime, YamlValue] {
    def encode(value: LocalTime): YamlValue = YamlValue.String(value.toString)
  }

  given Encoder[OffsetDateTime, YamlValue] = new Encoder[OffsetDateTime, YamlValue] {
    def encode(value: OffsetDateTime): YamlValue = YamlValue.String(value.toString)
  }

  given Encoder[ZonedDateTime, YamlValue] = new Encoder[ZonedDateTime, YamlValue] {
    def encode(value: ZonedDateTime): YamlValue = YamlValue.String(value.toString)
  }

  given Encoder[UUID, YamlValue] = new Encoder[UUID, YamlValue] {
    def encode(value: UUID): YamlValue = YamlValue.String(value.toString)
  }

  given [A] => (encoder: Encoder[A, YamlValue]) => Encoder[Option[A], YamlValue] =
    new Encoder[Option[A], YamlValue] {
      def encode(value: Option[A]): YamlValue = encodeEval(value).run()

      override def encodeEval(value: Option[A]): Eval[YamlValue] =
        Eval.defer {
          value match {
            case None => Eval.now(YamlValue.Null)
            case Some(a) => encoder.encodeEval(a)
          }
        }
    }

  given [A] => (encoder: Encoder[A, YamlValue]) => Encoder[List[A], YamlValue] =
    new Encoder[List[A], YamlValue] {
      def encode(value: List[A]): YamlValue = encodeEval(value).run()

      override def encodeEval(value: List[A]): Eval[YamlValue] =
        Encoder.drainElements[A, YamlValue, YamlValue](
          value,
          encoder.encodeEval,
          finish = YamlValue.Sequence.apply
        )
    }

  given [A] => (encoder: Encoder[A, YamlValue]) => Encoder[Seq[A], YamlValue] =
    new Encoder[Seq[A], YamlValue] {
      def encode(value: Seq[A]): YamlValue = encodeEval(value).run()

      override def encodeEval(value: Seq[A]): Eval[YamlValue] =
        Encoder.drainElements[A, YamlValue, YamlValue](
          value.toList,
          encoder.encodeEval,
          finish = YamlValue.Sequence.apply
        )
    }

  given [A] => (encoder: Encoder[A, YamlValue]) => Encoder[Vector[A], YamlValue] =
    new Encoder[Vector[A], YamlValue] {
      def encode(value: Vector[A]): YamlValue = encodeEval(value).run()

      override def encodeEval(value: Vector[A]): Eval[YamlValue] =
        Encoder.drainElements[A, YamlValue, YamlValue](
          value.toList,
          encoder.encodeEval,
          finish = YamlValue.Sequence.apply
        )
    }

  given [A] => (encoder: Encoder[A, YamlValue]) => Encoder[Map[String, A], YamlValue] =
    new Encoder[Map[String, A], YamlValue] {
      def encode(value: Map[String, A]): YamlValue = encodeEval(value).run()

      override def encodeEval(value: Map[String, A]): Eval[YamlValue] =
        Encoder.drainElements[(String, A), (String, YamlValue), YamlValue](
          value.toList,
          encodeElem = { case (key, a) => encoder.encodeEval(a).map(fv => key -> fv) },
          finish = entries => YamlValue.Mapping(entries.toMap)
        )
    }
}
