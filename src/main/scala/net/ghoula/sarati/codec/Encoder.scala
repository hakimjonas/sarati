package net.ghoula.sarati.codec

import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.quoted.*

trait Encoder[-A, To] {

  def encode(value: A): To

  /** The same encode as an [[Eval]] program, so nesting drains iteratively instead of on the JVM
    * call stack. Leaf encoders keep the default; derived and composite encoders override it.
    * Calling [[encode]] runs it.
    */
  def encodeEval(value: A): Eval[To] = Eval.now(encode(value))

  def contramap[B](f: B => A): Encoder[B, To] = {
    val self = this
    new Encoder[B, To] {
      def encode(value: B): To = encodeEval(value).run()

      override def encodeEval(value: B): Eval[To] = self.encodeEval(f(value))
    }
  }
}

object Encoder {

  def apply[A, To](using encoder: Encoder[A, To]): Encoder[A, To] = encoder

  /** Hybrid element drain for the composite encoders ([[JsonEncoders]]): strict (`Eval.Done`)
    * elements fold `@tailrec` with zero trampoline nodes; deferred elements suspend one `map` and
    * are drained by the surrounding trampoline, keeping every depth axis heap-bounded.
    */
  private[codec] def drainElements[E, F, To](
    elements: List[E],
    encodeElem: E => Eval[F],
    finish: List[F] => To
  ): Eval[To] = {

    def drain(remaining: List[E], acc: List[F]): Eval[To] = {
      @tailrec
      def strict(remaining: List[E], acc: List[F]): Either[(List[E], Eval[F], List[F]), To] =
        remaining match {
          case Nil => Right(finish(acc.reverse))
          case head :: tail =>
            encodeElem(head) match {
              case Eval.Done(fv) => strict(tail, fv :: acc)
              case deferred => Left((tail, deferred, acc))
            }
        }

      strict(remaining, acc) match {
        case Right(to) => Eval.now(to)
        case Left((rest, deferred, acc)) =>
          deferred.flatMap(fv => drain(rest, fv :: acc))
      }
    }

    drain(elements, Nil)
  }

  inline def derived[A, To](using m: Mirror.ProductOf[A], ft: FieldTransformer): Encoder[A, To] =
    ${ deriveEncoderImpl[A, To]('m, 'ft) }

  /** True when the summoned transformer is statically the identity — [[IdentityFieldTransformer]]
    * itself or the `FieldTransformer.default` given — in which case the generated code skips the
    * rename and inclusion checks entirely.
    */
  private def isIdentityTransformer(ft: Expr[FieldTransformer])(using Quotes): Boolean = {
    import quotes.reflect.*
    val term = ft.asTerm
    val sym = term.symbol
    val bySym =
      if sym.equals(Symbol.noSymbol) then false
      else
        sym.equals(Symbol.requiredModule("net.ghoula.sarati.codec.IdentityFieldTransformer")) ||
        (sym.owner
          .equals(Symbol.requiredModule("net.ghoula.sarati.codec.FieldTransformer")) && sym.name == "default")
    bySym || {
      val code = term.show(using Printer.TreeCode)
      code.equals("IdentityFieldTransformer") || code.equals("FieldTransformer.default")
    }
  }

  /** Inline-metaprogramming expansion behind [[derived]]: emits one encode step per constructor
    * field. Each field is read through a direct, type-safe reference (`value.fieldName` — `A` is
    * the concrete case class, so no cast and no `productElement` boxing), encoded with the summoned
    * `Encoder[fieldType, To]` — no erasure to `Any` — keyed through the [[FieldTransformer]]'s
    * rename, and dropped entirely when its `shouldIncludeField` returns false. For the
    * statically-identity transformer the checks are elided and labels pass through unchanged.
    */
  def deriveEncoderImpl[A: Type, To: Type](
    @annotation.unused m: Expr[Mirror.ProductOf[A]],
    ft: Expr[FieldTransformer]
  )(using Quotes): Expr[Encoder[A, To]] = {
    import quotes.reflect.*

    val ftExpr = ft
    val astBuilderExpr = Expr
      .summon[AstBuilder[To]]
      .getOrElse(
        report.errorAndAbort(
          s"Cannot derive Encoder for ${TypeRepr.of[A].show}: " +
            s"no AstBuilder[${TypeRepr.of[To].show}] instance found. " +
            "Supported types: JsonValue, TomlValue, YamlValue, XmlNode."
        )
      )

    val fromSymbol = TypeRepr.of[A].typeSymbol
    val fieldSymbols = fromSymbol.primaryConstructor.paramSymss.flatten
    val labels = fieldSymbols.map(_.name)

    def encodeEvalBody(value: Expr[A])(using Quotes): Expr[Eval[To]] = {
      val fieldEvals: List[Expr[Eval[Option[(String, To)]]]] = fieldSymbols.zipWithIndex.map { case (field, idx) =>
        val fieldType = field.tree match {
          case v: ValDef => v.tpt.tpe
          case _ => report.errorAndAbort(s"Expected ValDef for field ${field.name}")
        }
        fieldType.asType match {
          case '[ft] =>
            val enc = Expr
              .summon[Encoder[ft, To]]
              .getOrElse(
                report.errorAndAbort(
                  s"Cannot derive Encoder for ${TypeRepr.of[A].show}: " +
                    s"missing Encoder[${fieldType.show}, ${TypeRepr.of[To].show}]. " +
                    "Please provide a given instance."
                )
              )
            val labelExpr = Expr(labels(idx))
            // Direct field reference: A is the concrete case class, so the selection is
            // type-safe without any cast (no productElement boxing, no per-field asInstanceOf).
            val fieldValue = Select.unique(value.asTerm, field.name).asExprOf[ft]
            // Field-level key construction: the transformer renames the label and may exclude the
            // field entirely (`shouldIncludeField`). For the statically-identity transformer the
            // whole check is elided and the label is used as-is.
            val keyExpr: Expr[String] =
              if isIdentityTransformer(ftExpr) then labelExpr
              else '{ $ftExpr.transformFieldName($labelExpr) }
            val included: Expr[Boolean] =
              if isIdentityTransformer(ftExpr) then '{ true } else '{ $ftExpr.shouldIncludeField($labelExpr) }
            '{
              $enc
                .encodeEval($fieldValue)
                .map { fv =>
                  if $included then Some(($keyExpr, fv)) else None
                }
            }
        }
      }

      '{
        Eval
          .sequence(${ Expr.ofList(fieldEvals) })
          .map(_.flatten.toMap)
          .map($astBuilderExpr.createObject)
      }
    }

    '{
      new Encoder[A, To] {
        def encode(value: A): To = encodeEval(value).run()
        override def encodeEval(value: A): Eval[To] = ${ encodeEvalBody('value) }
      }
    }
  }
}
