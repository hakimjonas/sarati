package net.ghoula.sarati.codec

import scala.annotation.tailrec

/** Stack-safe trampoline for derived and composite codecs.
  *
  * Derived codecs recurse once per nesting level of the value being (de)serialized; on the JVM call
  * stack that recursion overflows on deeply nested values — the recursive binary codec could not
  * run the 10,000-deep benchmark at all, while the `Eval`-based layer completes it (see
  * `benchmarks/results`). Reifying the work as an [[Eval]] program and draining it with [[run]]
  * bounds the depth by heap instead.
  *
  * Combinators are pure; [[run]] is the single consuming operation, a `@tailrec` interpreter over
  * an immutable continuation stack.
  */
enum Eval[+A] {

  case Done[A](value: A) extends Eval[A]
  case More[A](thunk: () => Eval[A]) extends Eval[A]
  case FlatMap[A, B](fa: Eval[A], f: A => Eval[B]) extends Eval[B]

  def flatMap[B](f: A => Eval[B]): Eval[B] = FlatMap(this, f)

  def map[B](f: A => B): Eval[B] = FlatMap(this, (a: A) => Done(f(a)))

  /** Drains this program iteratively (constant call stack, heap-bounded). */
  final def run(): A = {
    @tailrec def loop(cur: Eval[Any], stack: List[Any => Eval[Any]]): A = cur match {
      case Done(a) =>
        stack match {
          case Nil => a.asInstanceOf[A] // scalafix:ok DisableSyntax.asInstanceOf
          case f :: rest => loop(f(a), rest)
        }
      case More(thunk) => loop(thunk(), stack)
      case FlatMap(fa, f) =>
        loop(fa, f.asInstanceOf[Any => Eval[Any]] :: stack) // scalafix:ok DisableSyntax.asInstanceOf
    }
    loop(this, Nil)
  }
}

object Eval {

  def now[A](a: A): Eval[A] = Done(a)

  def defer[A](a: => Eval[A]): Eval[A] = More(() => a)

  /** Sequences the programs left-to-right. The right-associated fold keeps [[run]]'s continuation
    * stack shallow for long lists.
    */
  def sequence[A](evals: List[Eval[A]]): Eval[List[A]] =
    evals.foldRight(now(List.empty[A])) { (e, acc) =>
      e.flatMap((a: A) => acc.map((as: List[A]) => a :: as))
    }
}
