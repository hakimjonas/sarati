package net.ghoula.sarati

/** Parse/decode outcome that can succeed with accumulated errors.
  *
  * [[Success]] is a clean result; [[Partial]] carries a usable value alongside the errors recovered
  * from (resilient decoding — e.g. a struct whose `Option` fields were absent but whose required
  * fields all resolved); [[Failure]] carries only errors and the furthest input position reached.
  * `consumed` counts consumed input items (bytes for binary formats).
  */
enum Result[+E, +A] {
  case Success(value: A, consumed: Int)
  case Partial(value: A, errors: List[E], consumed: Int)
  case Failure(errors: List[E], furthest: Location)
}

object Result {

  /** Conversions and predicates; `toOption` and `toEither` also return the [[Partial]] value. */
  extension [E, A](result: Result[E, A]) {

    inline def isSuccess: Boolean = result match {
      case Result.Success(_, _) => true
      case Result.Partial(_, _, _) => false
      case Result.Failure(_, _) => false
    }

    inline def isPartial: Boolean = result match {
      case Result.Partial(_, _, _) => true
      case _ => false
    }

    inline def isFailure: Boolean = result match {
      case Result.Failure(_, _) => true
      case _ => false
    }

    inline def toOption: Option[A] = result match {
      case Result.Success(value, _) => Some(value)
      case Result.Partial(value, _, _) => Some(value)
      case Result.Failure(_, _) => None
    }

    inline def toEither: Either[List[E], A] = result match {
      case Result.Success(value, _) => Right(value)
      case Result.Partial(value, _, _) => Right(value)
      case Result.Failure(errors, _) => Left(errors)
    }

    inline def errors: List[E] = result match {
      case Result.Success(_, _) => List.empty
      case Result.Partial(_, errs, _) => errs
      case Result.Failure(errs, _) => errs
    }
  }
}
