package net.ghoula.sarati.codec

import java.time.format.DateTimeParseException
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime}
import java.util.UUID

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.json.JsonValue

/** [[Decoder]] instances from [[JsonValue]] to primitives, `java.time` types, `UUID`, and the
  * common collection shapes.
  *
  * Typing is strict: a `Number` decodes to `Int`/`Long`/`Byte`/`Short` only when it is whole and
  * inside the target range, and string-typed targets (`String`, the `java.time` types, `UUID`)
  * decode only from `JsonValue.Str` with a parseable value — no coercions across JSON types. `Null`
  * decodes through the `Option` decoder only; decoding `Null` as a required value fails. The
  * `BigInt` decoder goes through `BigDecimal`, so whole doubles beyond `Long` range decode to their
  * exact double value rather than failing or wrapping.
  */
object JsonDecoders {

  given Decoder[JsonValue, String] = new Decoder[JsonValue, String] {
    def decode(value: JsonValue): Result[DecodeError, String] = value match {
      case JsonValue.Str(s) => Result.Success(s, 0)
      case JsonValue.Null =>
        Result.Failure(
          List(DecodeError.TypeMismatch("String", "Null", (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
      case JsonValue.Bool(b) =>
        Result.Failure(
          List(DecodeError.TypeMismatch("String", s"Boolean($b)", (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
      case JsonValue.Number(n, _) =>
        Result.Failure(
          List(DecodeError.TypeMismatch("String", s"Number($n)", (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
      case JsonValue.Array(_) =>
        Result.Failure(
          List(DecodeError.TypeMismatch("String", "Array", (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
      case JsonValue.Object(_) =>
        Result.Failure(
          List(DecodeError.TypeMismatch("String", "Object", (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, Int] = new Decoder[JsonValue, Int] {
    def decode(value: JsonValue): Result[DecodeError, Int] = value match {
      case JsonValue.Number(n, _) if n.isWhole && n >= Int.MinValue && n <= Int.MaxValue =>
        Result.Success(n.toInt, 0)
      case JsonValue.Number(n, _) if !n.isWhole =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Int", s"Number($n) - not a whole number", (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
      case JsonValue.Number(n, _) =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Int", s"Number($n) - out of range", (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
      case other =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Int", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, Long] = new Decoder[JsonValue, Long] {
    def decode(value: JsonValue): Result[DecodeError, Long] = value match {
      case JsonValue.Number(n, _) if n.isWhole && n >= Long.MinValue && n <= Long.MaxValue =>
        Result.Success(n.toLong, 0)
      case JsonValue.Number(n, _) if !n.isWhole =>
        Result.Failure(
          List(
            DecodeError.TypeMismatch("Long", s"Number($n) - not a whole number", (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
      case JsonValue.Number(n, _) =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Long", s"Number($n) - out of range", (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
      case other =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Long", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, Double] = new Decoder[JsonValue, Double] {
    def decode(value: JsonValue): Result[DecodeError, Double] = value match {
      case JsonValue.Number(n, _) => Result.Success(n, 0)
      case other =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Double", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, Boolean] = new Decoder[JsonValue, Boolean] {
    def decode(value: JsonValue): Result[DecodeError, Boolean] = value match {
      case JsonValue.Bool(b) => Result.Success(b, 0)
      case other =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Boolean", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, Byte] = new Decoder[JsonValue, Byte] {
    def decode(value: JsonValue): Result[DecodeError, Byte] = value match {
      case JsonValue.Number(n, _) if n.isWhole && n >= Byte.MinValue && n <= Byte.MaxValue =>
        Result.Success(n.toByte, 0)
      case JsonValue.Number(n, _) =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Byte", s"Number($n) - out of range or not whole", (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
      case other =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Byte", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, Short] = new Decoder[JsonValue, Short] {
    def decode(value: JsonValue): Result[DecodeError, Short] = value match {
      case JsonValue.Number(n, _) if n.isWhole && n >= Short.MinValue && n <= Short.MaxValue =>
        Result.Success(n.toShort, 0)
      case JsonValue.Number(n, _) =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Short", s"Number($n) - out of range or not whole", (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
      case other =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Short", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, Float] = new Decoder[JsonValue, Float] {
    def decode(value: JsonValue): Result[DecodeError, Float] = value match {
      case JsonValue.Number(n, _) => Result.Success(n.toFloat, 0)
      case other =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Float", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, BigInt] = new Decoder[JsonValue, BigInt] {
    def decode(value: JsonValue): Result[DecodeError, BigInt] = value match {
      case JsonValue.Number(n, _) if n.isWhole =>
        BigDecimal(n).toBigIntExact match {
          case Some(big) => Result.Success(big, 0)
          case None =>
            Result.Failure(
              List(
                DecodeError.TypeMismatch("BigInt", s"Number($n) - not integral", (line = 1, column = 1, offset = 0))
              ),
              (line = 1, column = 1, offset = 0)
            )
        }
      case JsonValue.Number(n, _) =>
        Result.Failure(
          List(
            DecodeError.TypeMismatch("BigInt", s"Number($n) - not a whole number", (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
      case other =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("BigInt", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, BigDecimal] = new Decoder[JsonValue, BigDecimal] {
    def decode(value: JsonValue): Result[DecodeError, BigDecimal] = value match {
      case JsonValue.Number(n, _) => Result.Success(BigDecimal(n), 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("BigDecimal", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, Instant] = new Decoder[JsonValue, Instant] {
    def decode(value: JsonValue): Result[DecodeError, Instant] = value match {
      case JsonValue.Str(s) =>
        try Result.Success(Instant.parse(s), 0)
        catch {
          case _: DateTimeParseException =>
            Result.Failure(
              List(
                DecodeError.TypeMismatch(
                  "Instant (ISO-8601)",
                  s"String($s) - invalid format",
                  (line = 1, column = 1, offset = 0)
                )
              ),
              (line = 1, column = 1, offset = 0)
            )
        }
      case other =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("Instant", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, LocalDate] = new Decoder[JsonValue, LocalDate] {
    def decode(value: JsonValue): Result[DecodeError, LocalDate] = value match {
      case JsonValue.Str(s) =>
        try Result.Success(LocalDate.parse(s), 0)
        catch {
          case _: DateTimeParseException =>
            Result.Failure(
              List(
                DecodeError.TypeMismatch(
                  "LocalDate (ISO-8601)",
                  s"String($s) - invalid format",
                  (line = 1, column = 1, offset = 0)
                )
              ),
              (line = 1, column = 1, offset = 0)
            )
        }
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("LocalDate", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, LocalDateTime] = new Decoder[JsonValue, LocalDateTime] {
    def decode(value: JsonValue): Result[DecodeError, LocalDateTime] = value match {
      case JsonValue.Str(s) =>
        try Result.Success(LocalDateTime.parse(s), 0)
        catch {
          case _: DateTimeParseException =>
            Result.Failure(
              List(
                DecodeError.TypeMismatch(
                  "LocalDateTime (ISO-8601)",
                  s"String($s) - invalid format",
                  (line = 1, column = 1, offset = 0)
                )
              ),
              (line = 1, column = 1, offset = 0)
            )
        }
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("LocalDateTime", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, LocalTime] = new Decoder[JsonValue, LocalTime] {
    def decode(value: JsonValue): Result[DecodeError, LocalTime] = value match {
      case JsonValue.Str(s) =>
        try Result.Success(LocalTime.parse(s), 0)
        catch {
          case _: DateTimeParseException =>
            Result.Failure(
              List(
                DecodeError.TypeMismatch(
                  "LocalTime (ISO-8601)",
                  s"String($s) - invalid format",
                  (line = 1, column = 1, offset = 0)
                )
              ),
              (line = 1, column = 1, offset = 0)
            )
        }
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("LocalTime", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, OffsetDateTime] = new Decoder[JsonValue, OffsetDateTime] {
    def decode(value: JsonValue): Result[DecodeError, OffsetDateTime] = value match {
      case JsonValue.Str(s) =>
        try Result.Success(OffsetDateTime.parse(s), 0)
        catch {
          case _: DateTimeParseException =>
            Result.Failure(
              List(
                DecodeError.TypeMismatch(
                  "OffsetDateTime (ISO-8601)",
                  s"String($s) - invalid format",
                  (line = 1, column = 1, offset = 0)
                )
              ),
              (line = 1, column = 1, offset = 0)
            )
        }
      case other =>
        Result.Failure(
          List(
            DecodeError.TypeMismatch("OffsetDateTime", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, ZonedDateTime] = new Decoder[JsonValue, ZonedDateTime] {
    def decode(value: JsonValue): Result[DecodeError, ZonedDateTime] = value match {
      case JsonValue.Str(s) =>
        try Result.Success(ZonedDateTime.parse(s), 0)
        catch {
          case _: DateTimeParseException =>
            Result.Failure(
              List(
                DecodeError.TypeMismatch(
                  "ZonedDateTime (ISO-8601)",
                  s"String($s) - invalid format",
                  (line = 1, column = 1, offset = 0)
                )
              ),
              (line = 1, column = 1, offset = 0)
            )
        }
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("ZonedDateTime", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given Decoder[JsonValue, UUID] = new Decoder[JsonValue, UUID] {
    def decode(value: JsonValue): Result[DecodeError, UUID] = value match {
      case JsonValue.Str(s) =>
        try Result.Success(UUID.fromString(s), 0)
        catch {
          case _: IllegalArgumentException =>
            Result.Failure(
              List(
                DecodeError.TypeMismatch("UUID", s"String($s) - invalid format", (line = 1, column = 1, offset = 0))
              ),
              (line = 1, column = 1, offset = 0)
            )
        }
      case other =>
        Result.Failure(
          List(
            DecodeError
              .TypeMismatch("UUID", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
          ),
          (line = 1, column = 1, offset = 0)
        )
    }
  }

  given [A] => (decoder: Decoder[JsonValue, A]) => Decoder[JsonValue, Option[A]] =
    new Decoder[JsonValue, Option[A]] {
      def decode(value: JsonValue): Result[DecodeError, Option[A]] = decodeEval(value).run()

      override def decodeEval(value: JsonValue): Eval[Result[DecodeError, Option[A]]] =
        Eval.defer {
          value match {
            case JsonValue.Null => Eval.now(Result.Success(None, 0))
            case other =>
              decoder.decodeEval(other).map {
                case Result.Success(a, consumed) => Result.Success(Some(a), consumed)
                case Result.Partial(a, errors, consumed) => Result.Partial(Some(a), errors, consumed)
                case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
              }
          }
        }
    }

  given [A] => (decoder: Decoder[JsonValue, A]) => Decoder[JsonValue, List[A]] =
    new Decoder[JsonValue, List[A]] {
      def decode(value: JsonValue): Result[DecodeError, List[A]] = decodeEval(value).run()

      override def decodeEval(value: JsonValue): Eval[Result[DecodeError, List[A]]] =
        Eval.defer {
          value match {
            case JsonValue.Array(elements) =>
              Decoder.drainElements[JsonValue, A, List[A], List[A]](
                elements,
                decoder.decodeEval,
                emptyAcc = Nil,
                step = (_, a, es, acc, errs) => (a :: acc, es.reverse ::: errs),
                finish = (acc, errs) =>
                  if errs.isEmpty then Result.Success(acc.reverse, 0)
                  else Result.Partial(acc.reverse, errs.reverse, 0),
                onFail = es => Result.Failure(es, (line = 1, column = 1, offset = 0))
              )
            case other =>
              Eval.now(
                Result.Failure(
                  List(
                    DecodeError
                      .TypeMismatch("Array", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
                  ),
                  (line = 1, column = 1, offset = 0)
                )
              )
          }
        }
    }

  given [A] => (decoder: Decoder[JsonValue, A]) => Decoder[JsonValue, Seq[A]] =
    new Decoder[JsonValue, Seq[A]] {
      def decode(value: JsonValue): Result[DecodeError, Seq[A]] = decodeEval(value).run()

      override def decodeEval(value: JsonValue): Eval[Result[DecodeError, Seq[A]]] =
        summon[Decoder[JsonValue, List[A]]].decodeEval(value).map {
          case Result.Success(list, consumed) => Result.Success(list.toSeq, consumed)
          case Result.Partial(list, errors, consumed) => Result.Partial(list.toSeq, errors, consumed)
          case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        }
    }

  given [A] => (decoder: Decoder[JsonValue, A]) => Decoder[JsonValue, Vector[A]] =
    new Decoder[JsonValue, Vector[A]] {
      def decode(value: JsonValue): Result[DecodeError, Vector[A]] = decodeEval(value).run()

      override def decodeEval(value: JsonValue): Eval[Result[DecodeError, Vector[A]]] =
        summon[Decoder[JsonValue, List[A]]].decodeEval(value).map {
          case Result.Success(list, consumed) => Result.Success(list.toVector, consumed)
          case Result.Partial(list, errors, consumed) => Result.Partial(list.toVector, errors, consumed)
          case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        }
    }

  given [A] => (decoder: Decoder[JsonValue, A]) => Decoder[JsonValue, Map[String, A]] =
    new Decoder[JsonValue, Map[String, A]] {
      def decode(value: JsonValue): Result[DecodeError, Map[String, A]] = decodeEval(value).run()

      override def decodeEval(value: JsonValue): Eval[Result[DecodeError, Map[String, A]]] =
        Eval.defer {
          value match {
            case JsonValue.Object(fields) =>
              Decoder.drainElements[(String, JsonValue), A, Map[String, A], Map[String, A]](
                fields.toList,
                decodeElem = { case (_, fieldValue) => decoder.decodeEval(fieldValue) },
                emptyAcc = Map.empty[String, A],
                step = (kv, a, es, acc, errs) => (acc + (kv._1 -> a), es.reverse ::: errs),
                finish = (acc, errs) =>
                  if errs.isEmpty then Result.Success(acc, 0)
                  else Result.Partial(acc, errs.reverse, 0),
                onFail = es => Result.Failure(es, (line = 1, column = 1, offset = 0))
              )
            case other =>
              Eval.now(
                Result.Failure(
                  List(
                    DecodeError
                      .TypeMismatch("Object", jsonValueTypeName(other), (line = 1, column = 1, offset = 0))
                  ),
                  (line = 1, column = 1, offset = 0)
                )
              )
          }
        }
    }

  private def jsonValueTypeName(value: JsonValue): String = value match {
    case JsonValue.Null => "Null"
    case JsonValue.Bool(_) => "Boolean"
    case JsonValue.Number(_, _) => "Number"
    case JsonValue.Str(_) => "String"
    case JsonValue.Array(_) => "Array"
    case JsonValue.Object(_) => "Object"
  }
}
