package net.ghoula.sarati.codec

import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.quoted.*

import net.ghoula.sarati.*

trait Decoder[From, +To] {

  def decode(value: From): Result[DecodeError, To]

  /** The same decode as an [[Eval]] program, so nesting drains iteratively instead of on the JVM
    * call stack. Leaf decoders keep the default (a plain `decode` result); derived and composite
    * decoders override it with deferred programs. Calling [[decode]] runs it.
    */
  def decodeEval(value: From): Eval[Result[DecodeError, To]] = Eval.now(decode(value))

  def map[B](f: To => B): Decoder[From, B] = new Decoder[From, B] {
    def decode(value: From): Result[DecodeError, B] = decodeEval(value).run()

    override def decodeEval(value: From): Eval[Result[DecodeError, B]] =
      Decoder.this.decodeEval(value).map {
        case Result.Success(a, consumed) => Result.Success(f(a), consumed)
        case Result.Partial(a, errors, consumed) => Result.Partial(f(a), errors, consumed)
        case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
      }
  }

  def flatMap[B](f: To => Decoder[From, B]): Decoder[From, B] = new Decoder[From, B] {
    def decode(value: From): Result[DecodeError, B] = decodeEval(value).run()

    override def decodeEval(value: From): Eval[Result[DecodeError, B]] =
      Decoder.this.decodeEval(value).flatMap {
        case Result.Success(a, _) => f(a).decodeEval(value)
        case Result.Partial(a, errors, _) =>
          f(a).decodeEval(value).map {
            case Result.Success(b, consumed) => Result.Partial(b, errors, consumed)
            case Result.Partial(b, moreErrors, consumed) =>
              Result.Partial(b, errors ++ moreErrors, consumed)
            case Result.Failure(moreErrors, furthest) =>
              Result.Failure(errors ++ moreErrors, furthest)
          }
        case Result.Failure(errors, furthest) => Eval.now(Result.Failure(errors, furthest))
      }
  }
}

object Decoder {

  def apply[From, To](using decoder: Decoder[From, To]): Decoder[From, To] = decoder

  /** Hybrid element drain for the composite decoders ([[JsonDecoders]], [[TomlDecoders]], …).
    *
    * Elements whose decode is already computed (`Eval.Done` — the leaf decoders) are folded
    * `@tailrec` with zero trampoline nodes, so a wide list of primitives costs no more than the
    * element decodes themselves. Elements whose decode is a deferred program (derived or nested
    * composite decoders) suspend one `flatMap` and are drained by the surrounding trampoline, so
    * every depth axis — recursive products, nested composites — stays heap-bounded. The first
    * failing element short-circuits through `onFail` (its own errors; accumulated partial errors
    * are dropped, matching the documented composite contract).
    *
    * `step` folds one decoded element into the accumulator; it is called only for `Success` and
    * `Partial` (a `Partial` carries its value and its own errors).
    */
  private[codec] def drainElements[E, A, Acc, R](
    elements: List[E],
    decodeElem: E => Eval[Result[DecodeError, A]],
    emptyAcc: Acc,
    step: (E, A, List[DecodeError], Acc, List[DecodeError]) => (Acc, List[DecodeError]),
    finish: (Acc, List[DecodeError]) => Result[DecodeError, R],
    onFail: List[DecodeError] => Result[DecodeError, R]
  ): Eval[Result[DecodeError, R]] = {

    def drain(remaining: List[E], acc: Acc, errs: List[DecodeError]): Eval[Result[DecodeError, R]] = {
      def applyStep(
        head: E,
        r: Result[DecodeError, A],
        acc: Acc,
        errs: List[DecodeError]
      ): Either[Result[DecodeError, R], (Acc, List[DecodeError])] =
        r match {
          case Result.Success(a, _) => Right(step(head, a, Nil, acc, errs))
          case Result.Partial(a, es, _) => Right(step(head, a, es, acc, errs))
          case Result.Failure(es, _) => Left(onFail(es))
        }

      @tailrec
      def strict(
        remaining: List[E],
        acc: Acc,
        errs: List[DecodeError]
      ): Either[(E, List[E], Eval[Result[DecodeError, A]]), Result[DecodeError, R]] =
        remaining match {
          case Nil => Right(finish(acc, errs))
          case head :: tail =>
            decodeElem(head) match {
              case Eval.Done(r) =>
                applyStep(head, r, acc, errs) match {
                  case Right((acc2, errs2)) => strict(tail, acc2, errs2)
                  case Left(failed) => Right(failed)
                }
              case deferred => Left((head, tail, deferred))
            }
        }

      strict(remaining, acc, errs) match {
        case Right(done) => Eval.now(done)
        case Left((head, rest, deferred)) =>
          deferred.flatMap { r =>
            applyStep(head, r, acc, errs) match {
              case Right((acc2, errs2)) => drain(rest, acc2, errs2)
              case Left(failed) => Eval.now(failed)
            }
          }
      }
    }

    drain(elements, emptyAcc, Nil)
  }

  inline def derived[From, To](using m: Mirror.Of[To], ft: FieldTransformer): Decoder[From, To] =
    inline m match {
      case p: Mirror.ProductOf[To] => derivedProduct[From, To](using p, ft)
      case s: Mirror.SumOf[To] => derivedSum[From, To](using s, ft)
    }

  inline def derivedProduct[From, To](using m: Mirror.ProductOf[To], ft: FieldTransformer): Decoder[From, To] =
    ${ deriveDecoderImpl[From, To]('m, 'ft) }

  inline def derivedSum[From, To](using m: Mirror.SumOf[To], ft: FieldTransformer): Decoder[From, To] =
    ${ deriveSumDecoderImpl[From, To, m.MirroredElemTypes, m.MirroredElemLabels]('m, 'ft) }

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

  /** Inline-metaprogramming expansion behind [[derived]]: emits one decode step per constructor
    * field. Each step reads the field's AST value through the summoned `AstStruct[From]` (honoring
    * the [[FieldTransformer]]'s rename and exclusion, elided entirely when the transformer is
    * statically the identity), decodes it with the summoned `Decoder[From, fieldType]` — no erasure
    * to `Any` — and synthesizes `None` for absent `Option` fields while required fields report
    * [[DecodeError.MissingField]]. Field results join by plain covariance of [[Result]]; the only
    * weakly-typed step is `Mirror.fromProduct`, which correctly-written derivations satisfy by
    * construction.
    */
  def deriveDecoderImpl[From: Type, To: Type](
    m: Expr[Mirror.ProductOf[To]],
    ft: Expr[FieldTransformer]
  )(using Quotes): Expr[Decoder[From, To]] = {
    import quotes.reflect.*

    // `struct` is a stable given: splicing the reference at each field costs the same static
    // access a hoisted local would, and the field decoders are referenced only at decode time
    // (never during construction), so recursive case classes cannot self-reference mid-init.
    val structRef = Expr
      .summon[AstStruct[From]]
      .getOrElse(
        report.errorAndAbort(
          s"Decoder derivation requires an AstStruct[${TypeRepr.of[From].show}] instance. " +
            "Supported types: JsonValue, TomlValue, YamlValue, XmlNode."
        )
      )

    val toSymbol = TypeRepr.of[To].typeSymbol
    val fieldSymbols = toSymbol.primaryConstructor.paramSymss.flatten
    val labels = fieldSymbols.map(_.name)

    def decodeEvalBody(value: Expr[From])(using Quotes): Expr[Eval[Result[DecodeError, To]]] = {
      val fieldEvals: List[Expr[Eval[Result[DecodeError, Any]]]] = fieldSymbols.zipWithIndex.map { case (field, idx) =>
        val fieldType = field.tree match {
          case v: ValDef => v.tpt.tpe
          case _ => report.errorAndAbort(s"Expected ValDef for field ${field.name}")
        }
        val isOpt = fieldType.dealias match {
          case AppliedType(base, _) =>
            base.typeSymbol.equals(Symbol.requiredClass("scala.Option"))
          case _ => false
        }
        fieldType.asType match {
          case '[ft] =>
            val dec = Expr
              .summon[Decoder[From, ft]]
              .getOrElse(
                report.errorAndAbort(
                  s"Cannot derive Decoder for ${TypeRepr.of[To].show}: " +
                    s"missing Decoder[${TypeRepr.of[From].show}, ${fieldType.show}]. " +
                    "Please provide a given instance."
                )
              )
            val labelExpr = Expr(labels(idx))
            // The branches are generated per field: an `Option` field synthesizes `None` on a
            // missing key, a required field reports `MissingField` — no dead guards survive.
            // A field excluded by the transformer's `shouldIncludeField` is treated exactly like
            // a missing key; for the statically-identity transformer the whole check is elided.
            val keyExpr: Expr[String] =
              if isIdentityTransformer(ft) then labelExpr
              else '{ $ft.transformFieldName($labelExpr) }
            val included: Expr[Boolean] =
              if isIdentityTransformer(ft) then '{ true } else '{ $ft.shouldIncludeField($labelExpr) }
            if isOpt then
              '{
                if $included then
                  $structRef.getField($value, $keyExpr) match {
                    case Some(fieldValue) => $dec.decodeEval(fieldValue)
                    case None => Eval.now(Result.Success(None, 0))
                  }
                else Eval.now(Result.Success(None, 0))
              }
            else
              '{
                if $included then
                  $structRef.getField($value, $keyExpr) match {
                    case Some(fieldValue) => $dec.decodeEval(fieldValue)
                    case None =>
                      Eval.now(
                        Result.Failure(
                          List(
                            DecodeError.MissingField(
                              $labelExpr,
                              (line = 1, column = 1, offset = 0)
                            )
                          ),
                          (line = 1, column = 1, offset = 0)
                        )
                      )
                  }
                else
                  Eval.now(
                    Result.Failure(
                      List(
                        DecodeError.MissingField(
                          $labelExpr,
                          (line = 1, column = 1, offset = 0)
                        )
                      ),
                      (line = 1, column = 1, offset = 0)
                    )
                  )
              }
        }
      }

      '{
        Eval.defer {
          if ! $structRef.isStruct($value) then
            Eval.now(
              Result.Failure(
                List(
                  DecodeError.TypeMismatch(
                    $structRef.expectedName,
                    $structRef.actualName($value),
                    (line = 1, column = 1, offset = 0)
                  )
                ),
                (line = 1, column = 1, offset = 0)
              )
            )
          else
            Eval.sequence(${ Expr.ofList(fieldEvals) }).map { (results: List[Result[DecodeError, Any]]) =>
              val (values, errors, failed) =
                results.foldRight((List.empty[Any], List.empty[DecodeError], false)) {
                  case (Result.Success(v, _), (vs, es, f)) => (v :: vs, es, f)
                  case (Result.Partial(v, errs, _), (vs, es, f)) => (v :: vs, errs ::: es, f)
                  case (Result.Failure(errs, _), (vs, es, _)) => (vs, errs ::: es, true)
                }

              if failed then Result.Failure(errors, (line = 1, column = 1, offset = 0))
              else {
                val product = $m.fromProduct(Tuple.fromArray(values.toArray))
                if errors.isEmpty then Result.Success(product, 0)
                else Result.Partial(product, errors, 0)
              }
            }
        }
      }
    }

    '{
      new Decoder[From, To] {
        def decode(value: From): Result[DecodeError, To] = decodeEval(value).run()
        override def decodeEval(value: From): Eval[Result[DecodeError, To]] =
          Eval.defer { ${ decodeEvalBody('value) } }
      }
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

  def deriveSumDecoderImpl[From: Type, To: Type, Elems <: Tuple: Type, Labels <: Tuple: Type](
    @annotation.unused m: Expr[Mirror.SumOf[To]],
    ft: Expr[FieldTransformer]
  )(using Quotes): Expr[Decoder[From, To]] = {
    import quotes.reflect.*

    val structRef = Expr
      .summon[AstStruct[From]]
      .getOrElse(
        report.errorAndAbort(
          s"Decoder derivation requires an AstStruct[${TypeRepr.of[From].show}] instance. " +
            "Supported types: JsonValue, TomlValue, YamlValue, XmlNode."
        )
      )

    val stringDec = Expr
      .summon[Decoder[From, String]]
      .getOrElse(
        report.errorAndAbort(
          s"Decoder derivation for sum type ${TypeRepr.of[To].show} requires a Decoder[${TypeRepr.of[From].show}, String] instance."
        )
      )

    val intDec = Expr
      .summon[Decoder[From, Int]]
      .getOrElse(
        report.errorAndAbort(
          s"Decoder derivation for sum type ${TypeRepr.of[To].show} requires a Decoder[${TypeRepr.of[From].show}, Int] instance."
        )
      )

    val labels = getLabels[Labels]
    val elemTypes = getTypes[Elems]

    if labels.length != elemTypes.length then
      report.errorAndAbort(s"Mismatched label and elem counts for ${TypeRepr.of[To].show}")

    val isSimpleEnum = elemTypes.forall { tpe =>
      val sym = tpe.typeSymbol
      if sym.isClassDef && sym.primaryConstructor.exists then sym.primaryConstructor.paramSymss.flatten.isEmpty
      else true
    }

    def getCaseInstance(idx: Int): Expr[To] = {
      val elemType = elemTypes(idx)
      elemType.asType match {
        case '[ct] =>
          Expr.summon[ValueOf[ct]] match {
            case Some(vo) => '{ $vo.value.asInstanceOf[To] } // scalafix:ok DisableSyntax.asInstanceOf
            case None =>
              Expr.summon[Mirror.ProductOf[ct]] match {
                case Some(mp) =>
                  '{ $mp.fromProduct(EmptyTuple).asInstanceOf[To] } // scalafix:ok DisableSyntax.asInstanceOf
                case None =>
                  Ref(elemType.typeSymbol).asExprOf[To]
              }
          }
      }
    }

    if isSimpleEnum then {
      val expectedLabelsExpr = Expr(s"One of [${labels.mkString(", ")}]")

      def generateSimpleEnumBranches(
        str: Expr[String],
        consumed: Expr[Int],
        idx: Int
      ): Expr[Result[DecodeError, To]] = {
        if idx >= labels.length then {
          '{
            Result.Failure(
              List(
                DecodeError.TypeMismatch(
                  $expectedLabelsExpr,
                  $str,
                  (line = 1, column = 1, offset = 0)
                )
              ),
              (line = 1, column = 1, offset = 0)
            )
          }
        } else {
          val labelExpr = Expr(labels(idx))
          val caseValExpr = getCaseInstance(idx)
          '{
            if $str == $labelExpr then {
              Result.Success($caseValExpr, $consumed)
            } else {
              ${ generateSimpleEnumBranches(str, consumed, idx + 1) }
            }
          }
        }
      }

      '{
        new Decoder[From, To] {
          def decode(value: From): Result[DecodeError, To] = decodeEval(value).run()
          override def decodeEval(value: From): Eval[Result[DecodeError, To]] =
            Eval.defer {
              $stringDec.decodeEval(value).map {
                case Result.Success(str, consumed) =>
                  ${ generateSimpleEnumBranches('str, 'consumed, 0) }
                case Result.Partial(str, _, consumed) =>
                  ${ generateSimpleEnumBranches('str, 'consumed, 0) }
                case Result.Failure(errors, furthest) =>
                  Result.Failure(errors, furthest)
              }
            }
        }
      }
    } else {
      val discName: Expr[String] = discriminatorNameExpr
      val expectedTypesExpr = Expr(s"One of [${labels.mkString(", ")}]")
      val expectedOrdinalsExpr = Expr(s"Ordinal in 0 until ${labels.length}")

      def decodeCase(value: Expr[From], idx: Int): Expr[Eval[Result[DecodeError, To]]] = {
        val elemType = elemTypes(idx)
        val caseLabel = labels(idx)
        elemType.asType match {
          case '[ct] =>
            val sym = elemType.typeSymbol
            val hasParams =
              sym.isClassDef && sym.primaryConstructor.exists && sym.primaryConstructor.paramSymss.flatten.nonEmpty
            if !hasParams then {
              val caseValExpr = getCaseInstance(idx)
              '{ Eval.now(Result.Success($caseValExpr, 0)) }
            } else {
              val caseDec = Expr.summon[Decoder[From, ct]].getOrElse {
                val mCase = Expr
                  .summon[Mirror.ProductOf[ct]]
                  .getOrElse(
                    report.errorAndAbort(
                      s"Cannot derive Decoder for case $caseLabel of ${TypeRepr.of[To].show}: no Mirror.ProductOf found."
                    )
                  )
                deriveDecoderImpl[From, ct](mCase, ft)
              }
              '{
                $caseDec.decodeEval($value).map {
                  case Result.Success(c, consumed) =>
                    Result.Success(c.asInstanceOf[To], consumed) // scalafix:ok DisableSyntax.asInstanceOf
                  case Result.Partial(c, errors, consumed) =>
                    Result.Partial(c.asInstanceOf[To], errors, consumed) // scalafix:ok DisableSyntax.asInstanceOf
                  case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
                }
              }
            }
        }
      }

      def matchByOrdinal(value: Expr[From], ord: Expr[Int], idx: Int): Expr[Eval[Result[DecodeError, To]]] = {
        if idx >= labels.length then {
          '{
            Eval.now(
              Result.Failure(
                List(
                  DecodeError.TypeMismatch(
                    $expectedOrdinalsExpr,
                    $ord.toString,
                    (line = 1, column = 1, offset = 0)
                  )
                ),
                (line = 1, column = 1, offset = 0)
              )
            )
          }
        } else {
          val idxExpr = Expr(idx)
          '{
            if $ord == $idxExpr then {
              ${ decodeCase(value, idx) }
            } else {
              ${ matchByOrdinal(value, ord, idx + 1) }
            }
          }
        }
      }

      def matchByName(value: Expr[From], name: Expr[String], idx: Int): Expr[Eval[Result[DecodeError, To]]] = {
        if idx >= labels.length then {
          '{
            $name.toIntOption match {
              case Some(ord) if ord >= 0 && ord < ${ Expr(labels.length) } =>
                ${ matchByOrdinal(value, 'ord, 0) }
              case _ =>
                Eval.now(
                  Result.Failure(
                    List(
                      DecodeError.TypeMismatch(
                        $expectedTypesExpr,
                        $name,
                        (line = 1, column = 1, offset = 0)
                      )
                    ),
                    (line = 1, column = 1, offset = 0)
                  )
                )
            }
          }
        } else {
          val labelExpr = Expr(labels(idx))
          '{
            if $name == $labelExpr then {
              ${ decodeCase(value, idx) }
            } else {
              ${ matchByName(value, name, idx + 1) }
            }
          }
        }
      }

      '{
        new Decoder[From, To] {
          def decode(value: From): Result[DecodeError, To] = decodeEval(value).run()
          override def decodeEval(value: From): Eval[Result[DecodeError, To]] =
            Eval.defer {
              if ! $structRef.isStruct(value) then {
                Eval.now(
                  Result.Failure(
                    List(
                      DecodeError.TypeMismatch(
                        $structRef.expectedName,
                        $structRef.actualName(value),
                        (line = 1, column = 1, offset = 0)
                      )
                    ),
                    (line = 1, column = 1, offset = 0)
                  )
                )
              } else {
                $structRef.getField(value, $discName) match {
                  case Some(typeNode) =>
                    $stringDec.decode(typeNode) match {
                      case Result.Success(typeName, _) =>
                        ${ matchByName('value, 'typeName, 0) }
                      case _ =>
                        $intDec.decode(typeNode) match {
                          case Result.Success(ord, _) if ord >= 0 && ord < ${ Expr(labels.length) } =>
                            ${ matchByOrdinal('value, 'ord, 0) }
                          case _ =>
                            Eval.now(
                              Result.Failure(
                                List(
                                  DecodeError.TypeMismatch(
                                    $expectedTypesExpr,
                                    $structRef.actualName(typeNode),
                                    (line = 1, column = 1, offset = 0)
                                  )
                                ),
                                (line = 1, column = 1, offset = 0)
                              )
                            )
                        }
                    }
                  case None =>
                    $structRef.getField(value, "_ordinal") match {
                      case Some(ordNode) =>
                        $intDec.decode(ordNode) match {
                          case Result.Success(ord, _) if ord >= 0 && ord < ${ Expr(labels.length) } =>
                            ${ matchByOrdinal('value, 'ord, 0) }
                          case _ =>
                            Eval.now(
                              Result.Failure(
                                List(
                                  DecodeError.TypeMismatch(
                                    $expectedOrdinalsExpr,
                                    $structRef.actualName(ordNode),
                                    (line = 1, column = 1, offset = 0)
                                  )
                                ),
                                (line = 1, column = 1, offset = 0)
                              )
                            )
                        }
                      case None =>
                        Eval.now(
                          Result.Failure(
                            List(
                              DecodeError.MissingField(
                                $discName,
                                (line = 1, column = 1, offset = 0)
                              )
                            ),
                            (line = 1, column = 1, offset = 0)
                          )
                        )
                    }
                }
              }
            }
        }
      }
    }
  }
}
