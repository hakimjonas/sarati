package net.ghoula.sarati.codec

import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime}

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.toml.TomlValue

/** [[Decoder]] instances from [[TomlValue]] to primitives, `java.time` types, and collections.
  *
  * Integers are exact (`Long`); `Int`/`Byte`/`Short` decode only from `Integer` values in range,
  * `BigInt` exactly from `Integer`, `BigDecimal` from `Float` or `Integer`, and `Double`/`Float`
  * from `Float` or widened `Integer`. Wrong shapes fail with [[DecodeError.TypeMismatch]].
  * Datetimes decode from the corresponding TOML variants, with widening to coarser types (a
  * `DateTime` yields its `LocalDate`/`LocalTime`/`LocalDateTime` projections); there is no string
  * parsing.
  */
object TomlDecoders {

  private val defaultLoc: Location = (line = 1, column = 1, offset = 0)

  given Decoder[TomlValue, String] = new Decoder[TomlValue, String] {
    def decode(value: TomlValue): Result[DecodeError, String] = value match {
      case TomlValue.String(s) => Result.Success(s, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("String", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, Int] = new Decoder[TomlValue, Int] {
    def decode(value: TomlValue): Result[DecodeError, Int] = value match {
      case TomlValue.Integer(n) if n >= Int.MinValue && n <= Int.MaxValue =>
        Result.Success(n.toInt, 0)
      case TomlValue.Integer(n) =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Int", s"Integer($n) - out of range", defaultLoc)),
          defaultLoc
        )
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Int", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, Long] = new Decoder[TomlValue, Long] {
    def decode(value: TomlValue): Result[DecodeError, Long] = value match {
      case TomlValue.Integer(n) => Result.Success(n, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Long", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, Double] = new Decoder[TomlValue, Double] {
    def decode(value: TomlValue): Result[DecodeError, Double] = value match {
      case TomlValue.Float(n) => Result.Success(n, 0)
      case TomlValue.Integer(n) => Result.Success(n.toDouble, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Double", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, Float] = new Decoder[TomlValue, Float] {
    def decode(value: TomlValue): Result[DecodeError, Float] = value match {
      case TomlValue.Float(n) => Result.Success(n.toFloat, 0)
      case TomlValue.Integer(n) => Result.Success(n.toFloat, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Float", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, Boolean] = new Decoder[TomlValue, Boolean] {
    def decode(value: TomlValue): Result[DecodeError, Boolean] = value match {
      case TomlValue.Boolean(b) => Result.Success(b, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Boolean", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, Byte] = new Decoder[TomlValue, Byte] {
    def decode(value: TomlValue): Result[DecodeError, Byte] = value match {
      case TomlValue.Integer(n) if n >= Byte.MinValue && n <= Byte.MaxValue => Result.Success(n.toByte, 0)
      case TomlValue.Integer(n) =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Byte", s"Integer($n) - out of range", defaultLoc)),
          defaultLoc
        )
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Byte", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, Short] = new Decoder[TomlValue, Short] {
    def decode(value: TomlValue): Result[DecodeError, Short] = value match {
      case TomlValue.Integer(n) if n >= Short.MinValue && n <= Short.MaxValue => Result.Success(n.toShort, 0)
      case TomlValue.Integer(n) =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Short", s"Integer($n) - out of range", defaultLoc)),
          defaultLoc
        )
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Short", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  /** Exact: TOML integers are `Long`-backed, so no precision is lost. */
  given Decoder[TomlValue, BigInt] = new Decoder[TomlValue, BigInt] {
    def decode(value: TomlValue): Result[DecodeError, BigInt] = value match {
      case TomlValue.Integer(n) => Result.Success(BigInt(n), 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("BigInt", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, BigDecimal] = new Decoder[TomlValue, BigDecimal] {
    def decode(value: TomlValue): Result[DecodeError, BigDecimal] = value match {
      case TomlValue.Float(n) => Result.Success(BigDecimal(n), 0)
      case TomlValue.Integer(n) => Result.Success(BigDecimal(n), 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("BigDecimal", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, OffsetDateTime] = new Decoder[TomlValue, OffsetDateTime] {
    def decode(value: TomlValue): Result[DecodeError, OffsetDateTime] = value match {
      case TomlValue.DateTime(dt) => Result.Success(dt, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("OffsetDateTime", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, LocalDateTime] = new Decoder[TomlValue, LocalDateTime] {
    def decode(value: TomlValue): Result[DecodeError, LocalDateTime] = value match {
      case TomlValue.LocalDateTime(dt) => Result.Success(dt, 0)
      case TomlValue.DateTime(dt) => Result.Success(dt.toLocalDateTime, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("LocalDateTime", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, LocalDate] = new Decoder[TomlValue, LocalDate] {
    def decode(value: TomlValue): Result[DecodeError, LocalDate] = value match {
      case TomlValue.LocalDate(d) => Result.Success(d, 0)
      case TomlValue.LocalDateTime(dt) => Result.Success(dt.toLocalDate, 0)
      case TomlValue.DateTime(dt) => Result.Success(dt.toLocalDate, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("LocalDate", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[TomlValue, LocalTime] = new Decoder[TomlValue, LocalTime] {
    def decode(value: TomlValue): Result[DecodeError, LocalTime] = value match {
      case TomlValue.LocalTime(t) => Result.Success(t, 0)
      case TomlValue.LocalDateTime(dt) => Result.Success(dt.toLocalTime, 0)
      case TomlValue.DateTime(dt) => Result.Success(dt.toLocalTime, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("LocalTime", tomlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given [A] => (decoder: Decoder[TomlValue, A]) => Decoder[TomlValue, Option[A]] =
    new Decoder[TomlValue, Option[A]] {
      def decode(value: TomlValue): Result[DecodeError, Option[A]] = decodeEval(value).run()

      override def decodeEval(value: TomlValue): Eval[Result[DecodeError, Option[A]]] =
        decoder.decodeEval(value).map {
          case Result.Success(a, consumed) => Result.Success(Some(a), consumed)
          case Result.Partial(a, errors, consumed) => Result.Partial(Some(a), errors, consumed)
          case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        }
    }

  given [A] => (decoder: Decoder[TomlValue, A]) => Decoder[TomlValue, List[A]] =
    new Decoder[TomlValue, List[A]] {
      def decode(value: TomlValue): Result[DecodeError, List[A]] = decodeEval(value).run()

      override def decodeEval(value: TomlValue): Eval[Result[DecodeError, List[A]]] =
        Eval.defer {
          value match {
            case TomlValue.Array(elements) =>
              Decoder.drainElements[TomlValue, A, List[A], List[A]](
                elements,
                decoder.decodeEval,
                emptyAcc = Nil,
                step = (_, a, es, acc, errs) => (a :: acc, es.reverse ::: errs),
                finish = (acc, errs) =>
                  if errs.isEmpty then Result.Success(acc.reverse, 0)
                  else Result.Partial(acc.reverse, errs.reverse, 0),
                onFail = es => Result.Failure(es, defaultLoc)
              )
            case other =>
              Eval.now(
                Result.Failure(
                  List(DecodeError.TypeMismatch("Array", tomlValueTypeName(other), defaultLoc)),
                  defaultLoc
                )
              )
          }
        }
    }

  given [A] => (decoder: Decoder[TomlValue, A]) => Decoder[TomlValue, Map[String, A]] =
    new Decoder[TomlValue, Map[String, A]] {
      def decode(value: TomlValue): Result[DecodeError, Map[String, A]] = decodeEval(value).run()

      override def decodeEval(value: TomlValue): Eval[Result[DecodeError, Map[String, A]]] =
        Eval.defer {
          value match {
            case TomlValue.InlineTable(pairs) =>
              Decoder.drainElements[(String, TomlValue), A, Map[String, A], Map[String, A]](
                pairs.toList,
                decodeElem = { case (_, fieldValue) => decoder.decodeEval(fieldValue) },
                emptyAcc = Map.empty[String, A],
                step = (kv, a, es, acc, errs) => (acc + (kv._1 -> a), es.reverse ::: errs),
                finish = (acc, errs) =>
                  if errs.isEmpty then Result.Success(acc, 0)
                  else Result.Partial(acc, errs.reverse, 0),
                onFail = es => Result.Failure(es, defaultLoc)
              )
            case other =>
              Eval.now(
                Result.Failure(
                  List(DecodeError.TypeMismatch("InlineTable", tomlValueTypeName(other), defaultLoc)),
                  defaultLoc
                )
              )
          }
        }
    }

  private def tomlValueTypeName(value: TomlValue): String = value match {
    case TomlValue.String(_) => "String"
    case TomlValue.Integer(_) => "Integer"
    case TomlValue.Float(_) => "Float"
    case TomlValue.Boolean(_) => "Boolean"
    case TomlValue.DateTime(_) => "DateTime"
    case TomlValue.LocalDateTime(_) => "LocalDateTime"
    case TomlValue.LocalDate(_) => "LocalDate"
    case TomlValue.LocalTime(_) => "LocalTime"
    case TomlValue.Array(_) => "Array"
    case TomlValue.InlineTable(_) => "InlineTable"
  }
}
