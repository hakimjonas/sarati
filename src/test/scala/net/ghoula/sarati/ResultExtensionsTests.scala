package net.ghoula.sarati

class ResultExtensionsTests extends munit.FunSuite {

  private val loc: Location = (line = 1, column = 1, offset = 0)

  test("Success.isSuccess is true") {
    val r: Result[String, Int] = Result.Success(42, 1)
    assert(r.isSuccess)
    assert(!r.isPartial)
    assert(!r.isFailure)
  }

  test("Partial.isPartial is true") {
    val r: Result[String, Int] = Result.Partial(42, List("warn"), 1)
    assert(!r.isSuccess)
    assert(r.isPartial)
    assert(!r.isFailure)
  }

  test("Failure.isFailure is true") {
    val r: Result[String, Int] = Result.Failure(List("err"), loc)
    assert(!r.isSuccess)
    assert(!r.isPartial)
    assert(r.isFailure)
  }

  test("toOption returns Some for Success") {
    val r: Result[String, Int] = Result.Success(42, 1)
    assertEquals(r.toOption, Some(42))
  }

  test("toOption returns Some for Partial") {
    val r: Result[String, Int] = Result.Partial(42, List("warn"), 1)
    assertEquals(r.toOption, Some(42))
  }

  test("toOption returns None for Failure") {
    val r: Result[String, Int] = Result.Failure(List("err"), loc)
    assertEquals(r.toOption, None)
  }

  test("toEither returns Right for Success") {
    val r: Result[String, Int] = Result.Success(42, 1)
    assertEquals(r.toEither, Right(42))
  }

  test("toEither returns Left for Failure") {
    val r: Result[String, Int] = Result.Failure(List("err"), loc)
    assertEquals(r.toEither, Left(List("err")))
  }

  test("errors is empty for Success") {
    val r: Result[String, Int] = Result.Success(42, 1)
    assertEquals(r.errors, List.empty)
  }

  test("errors returns list for Partial") {
    val r: Result[String, Int] = Result.Partial(42, List("a", "b"), 1)
    assertEquals(r.errors, List("a", "b"))
  }

  test("errors returns list for Failure") {
    val r: Result[String, Int] = Result.Failure(List("err"), loc)
    assertEquals(r.errors, List("err"))
  }
}
