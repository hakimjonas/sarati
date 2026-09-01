package net.ghoula.sarati.internal

import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

import net.ghoula.sarati.{SaratiCodec, SaratiError}

private[sarati] case class MissingCodec(
  fieldName: String,
  fieldType: String,
  suggestion: String
)

/** Inline-metaprogramming implementations (quotes and splices) behind `SaratiCodec.derived`:
  * compile-time generation of per-field encode/decode code from `Mirror` tuples, with missing-codec
  * diagnostics naming the field and the instance to provide.
  */
private[sarati] object SaratiDerivation {

  private def getLabels[Labels <: Tuple: Type](using q: Quotes): List[String] = {
    import q.reflect.*

    def extract[L <: Tuple: Type]: List[String] = Type.of[L] match {
      case '[EmptyTuple] => Nil
      case '[label *: rest] =>
        Type.of[label] match {
          case '[l] =>
            TypeRepr.of[l] match {
              case ConstantType(StringConstant(s)) => s :: extract[rest]
              case other =>
                report.errorAndAbort(
                  s"Invalid field label type: expected string literal, found ${other.show}."
                )
            }
        }
    }

    extract[Labels]
  }

  private def validateAllHaveCodecs[T: Type, Elems <: Tuple: Type](
    labels: List[String]
  )(using q: Quotes): Unit = {
    import q.reflect.*

    def collectMissing[E <: Tuple: Type](
      remainingLabels: List[String],
      acc: List[MissingCodec]
    ): List[MissingCodec] =
      Type.of[E] match {
        case '[EmptyTuple] => acc.reverse
        case '[h *: t] =>
          val label = remainingLabels.head
          val fieldTypeStr = Type.show[h]
          val hasCodec = Expr.summon[SaratiCodec[h]].isDefined
          val newAcc =
            hasCodec match {
              case true => acc
              case false =>
                MissingCodec(label, fieldTypeStr, s"given SaratiCodec[$fieldTypeStr] = ...") :: acc
            }
          collectMissing[t](remainingLabels.tail, newAcc)
      }

    val missing = collectMissing[Elems](labels, Nil)

    missing.nonEmpty match {
      case false => ()
      case true =>
        val header =
          s"Cannot derive SaratiCodec for ${Type.show[T]}: missing codecs for ${missing.length} field(s).\n"

        val details = missing.zipWithIndex.map { case (m, i) =>
          s"  ${i + 1}. Field '${m.fieldName}' of type ${m.fieldType}\n" +
            s"     Add: ${m.suggestion}"
        }
          .mkString("\n\n")

        val footer =
          "\n\nHint: Primitive codecs are provided automatically. " +
            "For custom types, derive them with `SaratiCodec.derived` or provide explicit instances."

        report.errorAndAbort(header + "\n" + details + footer, Position.ofMacroExpansion)
    }
  }

  private def generateFieldAccess[T: Type, H: Type](
    aExpr: Expr[T],
    label: String,
    index: Int
  )(using q: Quotes): Expr[H] = {
    import q.reflect.*

    val typeSymbol = TypeRepr.of[T].typeSymbol
    val hasFieldMember = typeSymbol.fieldMembers.exists(sym => sym.name == label)
    val isRegularTuple = TypeRepr.of[T] <:< TypeRepr.of[Tuple]

    (isRegularTuple, hasFieldMember) match {
      case (true, _) =>
        Select.unique(aExpr.asTerm, s"_${index + 1}").asExprOf[H]
      case (_, true) =>
        Select.unique(aExpr.asTerm, label).asExprOf[H]
      case (false, false) =>
        val indexExpr = Expr(index)
        '{
          // scalafix:off DisableSyntax.asInstanceOf
          // Derivation requires casts: case classes extend Product, type H is from the mirror
          $aExpr
            .asInstanceOf[Product]
            .productElement($indexExpr)
            .asInstanceOf[H]
          // scalafix:on DisableSyntax.asInstanceOf
        }
    }
  }

  // --- Product derivation ---

  private def generateProductEncode[T: Type, E <: Tuple: Type](
    aExpr: Expr[T],
    labels: List[String],
    index: Int
  )(using q: Quotes): List[Expr[Array[Byte]]] =
    Type.of[E] match {
      case '[EmptyTuple] => Nil
      case '[h *: t] =>
        val codec = Expr.summon[SaratiCodec[h]].get
        val label = labels.head
        val fieldAccess = generateFieldAccess[T, h](aExpr, label, index)
        val encodeExpr: Expr[Array[Byte]] = '{ $codec.encodeToArray($fieldAccess) }
        encodeExpr :: generateProductEncode[T, t](aExpr, labels.tail, index + 1)
    }

  private def generateProductDecode[E <: Tuple: Type](
    bytesExpr: Expr[IArray[Byte]],
    offsetExpr: Expr[Int],
    totalConsumedExpr: Expr[Int]
  )(using q: Quotes): Expr[Either[SaratiError, (Tuple, Int)]] =
    Type.of[E] match {
      case '[EmptyTuple] =>
        '{ Right((EmptyTuple, $totalConsumedExpr)) }
      case '[h *: t] =>
        val codec = Expr.summon[SaratiCodec[h]].get
        '{
          $codec.decode($bytesExpr, $offsetExpr) match {
            case Left(err) => Left(err)
            case Right((value, consumed)) =>
              ${
                generateProductDecode[t](
                  bytesExpr,
                  '{ $offsetExpr + consumed },
                  '{ $totalConsumedExpr + consumed }
                )
              } match {
                case Left(err) => Left(err)
                case Right((restTuple, totalConsumed)) =>
                  Right((value *: restTuple, totalConsumed))
              }
          }
        }
    }

  def deriveProductCodecImpl[T: Type, Elems <: Tuple: Type, Labels <: Tuple: Type](
    m: Expr[Mirror.ProductOf[T]]
  )(using q: Quotes): Expr[SaratiCodec[T]] = {
    val labels: List[String] = getLabels[Labels]
    validateAllHaveCodecs[T, Elems](labels)

    '{
      new SaratiCodec[T] {
        def encodeToArray(a: T): Array[Byte] = {
          val chunks: List[Array[Byte]] = ${
            Expr.ofList(generateProductEncode[T, Elems]('a, labels, 0))
          }
          ByteOps.concatArrays(chunks)
        }

        def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (T, Int)] =
          ${
            generateProductDecode[Elems]('bytes, 'offset, '{ 0 })
          } match {
            case Left(err) => Left(err)
            case Right((tuple, consumed)) =>
              Right(($m.fromProduct(tuple), consumed))
          }
      }
    }
  }

  // --- Sum derivation ---

  private def generateSumEncode[T: Type, E <: Tuple: Type](
    valueExpr: Expr[T],
    ordinalExpr: Expr[Int],
    idx: Int
  )(using q: Quotes): Expr[Array[Byte]] =
    Type.of[E] match {
      case '[EmptyTuple] =>
        '{ Array.empty[Byte] }
      case '[h *: t] =>
        val codec = Expr.summon[SaratiCodec[h]].get
        val idxExpr = Expr(idx)
        '{
          ($ordinalExpr == $idxExpr) match {
            case true =>
              $codec.encodeToArray(
                $valueExpr.asInstanceOf[h] // scalafix:ok DisableSyntax.asInstanceOf
              )
            case false => ${ generateSumEncode[T, t](valueExpr, ordinalExpr, idx + 1) }
          }
        }
    }

  private def generateSumDecode[T: Type, E <: Tuple: Type](
    bytesExpr: Expr[IArray[Byte]],
    offsetExpr: Expr[Int],
    ordinalConsumedExpr: Expr[Int],
    ordinalExpr: Expr[Int],
    idx: Int
  )(using q: Quotes): Expr[Either[SaratiError, (T, Int)]] =
    Type.of[E] match {
      case '[EmptyTuple] =>
        '{ Left(SaratiError.ParseError(s"Invalid ordinal: ${$ordinalExpr}")) }
      case '[h *: t] =>
        val codec = Expr.summon[SaratiCodec[h]].get
        val idxExpr = Expr(idx)
        '{
          ($ordinalExpr == $idxExpr) match {
            case true =>
              $codec.decode($bytesExpr, $offsetExpr) match {
                case Left(err) => Left(err)
                case Right((value, consumed)) =>
                  Right(
                    (
                      value.asInstanceOf[T], // scalafix:ok DisableSyntax.asInstanceOf
                      $ordinalConsumedExpr + consumed
                    )
                  )
              }
            case false =>
              ${
                generateSumDecode[T, t](
                  bytesExpr,
                  offsetExpr,
                  ordinalConsumedExpr,
                  ordinalExpr,
                  idx + 1
                )
              }
          }
        }
    }

  def deriveSumCodecImpl[T: Type, Elems <: Tuple: Type, Labels <: Tuple: Type](
    m: Expr[Mirror.SumOf[T]]
  )(using q: Quotes): Expr[SaratiCodec[T]] = {
    val labels: List[String] = getLabels[Labels]
    validateAllHaveCodecs[T, Elems](labels)

    '{
      new SaratiCodec[T] {
        def encodeToArray(value: T): Array[Byte] = {
          val ordinal: Int = $m.ordinal(value)
          val ordinalBytes: Array[Byte] = summon[SaratiCodec[Int]].encodeToArray(ordinal)
          val payloadBytes: Array[Byte] = ${
            generateSumEncode[T, Elems]('value, 'ordinal, 0)
          }
          ByteOps.concatArrays(List(ordinalBytes, payloadBytes))
        }

        def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (T, Int)] =
          summon[SaratiCodec[Int]].decode(bytes, offset) match {
            case Left(err) => Left(err)
            case Right((ordinal, consumed)) =>
              ${
                generateSumDecode[T, Elems](
                  'bytes,
                  '{ offset + consumed },
                  'consumed,
                  'ordinal,
                  0
                )
              }
          }
      }
    }
  }
}
