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

  inline def derived[A, To](using m: Mirror.Of[A], ft: FieldTransformer): Encoder[A, To] =
    inline m match {
      case p: Mirror.ProductOf[A] => derivedProduct[A, To](using p, ft)
      case s: Mirror.SumOf[A] => derivedSum[A, To](using s, ft)
    }

  inline def derivedProduct[A, To](using m: Mirror.ProductOf[A], ft: FieldTransformer): Encoder[A, To] =
    ${ deriveEncoderImpl[A, To]('m, 'ft) }

  inline def derivedSum[A, To](using m: Mirror.SumOf[A], ft: FieldTransformer): Encoder[A, To] =
    ${ deriveSumEncoderImpl[A, To, m.MirroredElemTypes, m.MirroredElemLabels]('m, 'ft) }

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
                report.errorAndAbort(s"Expected string literal in MirroredElemLabels, found ${other.show}")
            }
        }
    }
    extract[Labels]
  }

  private def getTypes[Types <: Tuple: Type](using q: Quotes): List[q.reflect.TypeRepr] = {
    import q.reflect.*
    def extract[T <: Tuple: Type]: List[TypeRepr] = Type.of[T] match {
      case '[EmptyTuple] => Nil
      case '[h *: t] => TypeRepr.of[h] :: extract[t]
    }
    extract[Types]
  }

  private def encodeProductFields[C: Type, To: Type](
    value: Expr[C],
    ft: Expr[FieldTransformer]
  )(using Quotes): List[Expr[Eval[Option[(String, To)]]]] = {
    import quotes.reflect.*

    val fromSymbol = TypeRepr.of[C].typeSymbol
    val fieldSymbols =
      if fromSymbol.isClassDef && fromSymbol.primaryConstructor.exists then {
        fromSymbol.primaryConstructor.paramSymss.flatten
      } else {
        Nil
      }
    val labels = fieldSymbols.map(_.name)

    fieldSymbols.zipWithIndex.map { case (field, idx) =>
      val fieldType =
        try {
          field.tree match {
            case v: ValDef => v.tpt.tpe
            case _ => TypeRepr.of[C].memberType(field)
          }
        } catch {
          case _: Exception => TypeRepr.of[C].memberType(field)
        }
      val resolvedType = fieldType match {
        case MethodType(_, _, res) => res
        case ByNameType(res) => res
        case other => other
      }
      resolvedType.asType match {
        case '[fType] =>
          val enc = Expr
            .summon[Encoder[fType, To]]
            .getOrElse(
              report.errorAndAbort(
                s"Cannot derive Encoder for ${TypeRepr.of[C].show}: " +
                  s"missing Encoder[${resolvedType.show}, ${TypeRepr.of[To].show}]. " +
                  "Please provide a given instance."
              )
            )
          val labelExpr = Expr(labels(idx))
          val fieldValue = Select.unique(value.asTerm, field.name).asExprOf[fType]
          val keyExpr: Expr[String] =
            if isIdentityTransformer(ft) then labelExpr
            else '{ $ft.transformFieldName($labelExpr) }
          val included: Expr[Boolean] =
            if isIdentityTransformer(ft) then '{ true } else '{ $ft.shouldIncludeField($labelExpr) }
          '{
            $enc
              .encodeEval($fieldValue)
              .map { fv =>
                if $included then Some(($keyExpr, fv)) else None
              }
          }
      }
    }
  }

  /** True when the summoned discriminator is statically the default — [[DefaultSumDiscriminator]]
    * itself or the `SumDiscriminator.default` given — in which case the generated code embeds the
    * `"_type"` literal instead of reading the property name at runtime.
    */
  private def isDefaultDiscriminator(disc: Expr[SumDiscriminator])(using Quotes): Boolean = {
    import quotes.reflect.*
    val term = disc.asTerm
    val sym = term.symbol
    if sym.equals(Symbol.noSymbol) then false
    else
      sym.equals(Symbol.requiredModule("net.ghoula.sarati.codec.DefaultSumDiscriminator")) ||
      (sym.owner
        .equals(Symbol.requiredModule("net.ghoula.sarati.codec.SumDiscriminator")) && sym.name == "default")
  }

  private def discriminatorNameExpr(using Quotes): Expr[String] = {
    val disc = Expr.summon[SumDiscriminator].getOrElse {
      import quotes.reflect.*
      report.errorAndAbort(
        "Sum derivation requires a SumDiscriminator instance (the default is SumDiscriminator.default)."
      )
    }
    if isDefaultDiscriminator(disc) then '{ "_type" } else '{ $disc.propertyName }
  }

  def deriveSumEncoderImpl[A: Type, To: Type, Elems <: Tuple: Type, Labels <: Tuple: Type](
    m: Expr[Mirror.SumOf[A]],
    ft: Expr[FieldTransformer]
  )(using Quotes): Expr[Encoder[A, To]] = {
    import quotes.reflect.*

    val astBuilderExpr = Expr
      .summon[AstBuilder[To]]
      .getOrElse(
        report.errorAndAbort(
          s"Cannot derive Encoder for ${TypeRepr.of[A].show}: " +
            s"no AstBuilder[${TypeRepr.of[To].show}] instance found. " +
            "Supported types: JsonValue, TomlValue, YamlValue, XmlNode."
        )
      )

    val labels = getLabels[Labels]
    val elemTypes = getTypes[Elems]

    if labels.length != elemTypes.length then {
      report.errorAndAbort(s"Mismatched label and elem counts for ${TypeRepr.of[A].show}")
    }

    val isSimpleEnum = elemTypes.forall { tpe =>
      val sym = tpe.typeSymbol
      if sym.isClassDef && sym.primaryConstructor.exists then {
        sym.primaryConstructor.paramSymss.flatten.isEmpty
      } else {
        true
      }
    }

    if isSimpleEnum then {
      val labelsArrayExpr = Expr(labels.toArray)
      '{
        val labelsArray: Array[String] = $labelsArrayExpr
        new Encoder[A, To] {
          def encode(value: A): To = {
            val ord = $m.ordinal(value)
            $astBuilderExpr.fromString(labelsArray(ord))
          }
          override def encodeEval(value: A): Eval[To] =
            Eval.now(encode(value))
        }
      }
    } else {
      val discName: Expr[String] = discriminatorNameExpr

      def encodeCase(value: Expr[A], idx: Int): Expr[Eval[To]] = {
        val elemType = elemTypes(idx)
        val caseLabel = labels(idx)
        val labelExpr = Expr(caseLabel)
        elemType.asType match {
          case '[ct] =>
            val casted = '{ $value.asInstanceOf[ct] } // scalafix:ok DisableSyntax.asInstanceOf
            val sym = elemType.typeSymbol
            val hasParams =
              sym.isClassDef && sym.primaryConstructor.exists && sym.primaryConstructor.paramSymss.flatten.nonEmpty
            if !hasParams then {
              '{
                Eval.now(
                  $astBuilderExpr.createObject(
                    Map($discName -> $astBuilderExpr.fromString($labelExpr))
                  )
                )
              }
            } else {
              val fieldEvals = encodeProductFields[ct, To](casted, ft)
              '{
                Eval
                  .sequence(${ Expr.ofList(fieldEvals) })
                  .map(_.flatten.toMap)
                  .map(fields => Map($discName -> $astBuilderExpr.fromString($labelExpr)) ++ fields)
                  .map($astBuilderExpr.createObject)
              }
            }
        }
      }

      def buildMatch(value: Expr[A], ordExpr: Expr[Int], idx: Int): Expr[Eval[To]] = {
        if idx >= elemTypes.length then {
          '{ throw new MatchError($ordExpr) }
        } else {
          val idxExpr = Expr(idx)
          '{
            if $ordExpr == $idxExpr then {
              ${ encodeCase(value, idx) }
            } else {
              ${ buildMatch(value, ordExpr, idx + 1) }
            }
          }
        }
      }

      '{
        new Encoder[A, To] {
          def encode(value: A): To = encodeEval(value).run()
          override def encodeEval(value: A): Eval[To] =
            Eval.defer {
              val ord = $m.ordinal(value)
              ${ buildMatch('value, 'ord, 0) }
            }
        }
      }
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

    def encodeEvalBody(value: Expr[A])(using Quotes): Expr[Eval[To]] = {
      val fieldEvals = encodeProductFields[A, To](value, ftExpr)
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
