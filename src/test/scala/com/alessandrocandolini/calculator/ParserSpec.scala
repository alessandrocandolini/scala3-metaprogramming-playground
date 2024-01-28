package com.alessandrocandolini.calculator

import cats.data.NonEmptyList
import cats.implicits.*
import com.alessandrocandolini.DefaultSuite
import com.alessandrocandolini.calculator.Parser.*

class ParserSpec extends DefaultSuite:

  test("boolean parser can parse true") {
    assertEquals(
      boolean.parseAll("true"),
      Some(true)
    )
  }

  test("boolean parser can parse false") {
    assertEquals(
      boolean.parseAll("false"),
      Some(false)
    )
  }

  test("boolean parser cannot parse capital case True") {
    assertEquals(
      boolean.parseAll("True"),
      None
    )
  }

  test("boolean case insensitive parser can parse true with uppercase letter") {
    assertEquals(
      booleanCaseInsensitive.parseAll("True"),
      Some(true)
    )
  }

  test("digit can parse a single digit as char") {
    assertEquals(
      digit.parseAll("1"),
      Some('1')
    )
  }

  test("digits can parse multiple digits") {
    assertEquals(
      digits.parseAll("1234"),
      Some(1234)
    )
  }

  test("can compose with other parsers") {

    val p = (digits, spaces *> string("hello")).tupled

    assertEquals(
      p.parseAll("1234  hello"),
      Some((1234, "hello"))
    )
  }

  test("spaces parser supports also 0 spaces") {

    val p = (digits, spaces *> string("hello")).tupled

    assertEquals(
      p.parseAll("1234hello"),
      Some((1234, "hello"))
    )
  }

  test("parenthesis succeeds when parenthesis are present") {

    val p = parenthesis(digits)

    assertEquals(
      p.parseAll("(1234)"),
      Some(1234)
    )
  }

  test("parenthesis fails when parenthesis are not present") {

    val p = parenthesis(digits)

    assertEquals(
      p.parseAll("1234"),
      None
    )
  }

  test("nested arenthesis succeeds when parenthesis are present twice") {

    val p = parenthesis(parenthesis(digits))

    assertEquals(
      p.parseAll("((1234))"),
      Some(1234)
    )
  }

  test("repeat fails on empty string") {

    val p = string("1234").repeat

    assertEquals(
      p.parseAll(""),
      None
    )
  }

  test("repeat fails on empty string") {

    val p: Parser[NonEmptyList[String]] = string("1234").repeat

    assertEquals(
      p.parseAll(""),
      None
    )
  }

  test("repeat0 succeeds on empty string") {

    val p: Parser[List[String]] = string("1234").repeat0

    assertEquals(
      p.parseAll(""),
      Some(List.empty)
    )
  }

  test("repeat succeeds on repeated string") {

    val p: Parser[NonEmptyList[String]] = string("1234").repeat

    assertEquals(
      p.parseAll("1234123412341234"),
      Some(NonEmptyList.fromListUnsafe(List.fill(4)("1234")))
    )
  }

  test("repeat fails on partially repeated string") {

    val p: Parser[NonEmptyList[String]] = string("1234").repeat

    assertEquals(
      p.parseAll("123412341234123"),
      None
    )
  }

  test("orElse works with partially overlapping strings") {
    val p = string("falser").orElse(string("false"))

    assertEquals(
      p.parseAll("false"),
      Some("false")
    )

    assertEquals(
      p.parseAll("falser"),
      Some("falser")
    )
  }

  test("orElse works with parenthesis and without parenthesis, even in presence of repeat") {
    val base = string("false").repeat
    val p    = parenthesis(base).orElse(base)

    assertEquals(
      p.parseAll("false"),
      Some(NonEmptyList.of("false"))
    )

    assertEquals(
      p.parseAll("(false)"),
      Some(NonEmptyList.of("false"))
    )

    assertEquals(
      p.parseAll("(falsefalsefalse)"),
      Some(NonEmptyList.of("false", "false", "false"))
    )
  }

  test("orElse with digits") {
    val p: Parser[NonEmptyList[Int]] = parenthesis(digits).orElse(digits).repeat

    assertEquals(
      p.parseAll("1234"),
      Some(NonEmptyList.of(1234))
    )

    assertEquals(
      p.parseAll("(1234)"),
      Some(NonEmptyList.of(1234))
    )

    assertEquals(
      p.parseAll("(1234)"),
      Some(NonEmptyList.of(1234))
    )

    assertEquals(
      p.parseAll("(1234)(2345)3456"),
      Some(NonEmptyList.of(1234, 2345, 3456))
    )
  }

  test("orElse with digits and spaces") {

    val d = spaces *> digits <* spaces

    val p: Parser[NonEmptyList[Int]] = parenthesis(d).orElse(d).repeat

    assertEquals(
      p.parseAll("1234 "),
      Some(NonEmptyList.of(1234))
    )

    assertEquals(
      p.parseAll("(1234)"),
      Some(NonEmptyList.of(1234))
    )

    assertEquals(
      p.parseAll("(1234)"),
      Some(NonEmptyList.of(1234))
    )

    assertEquals(
      p.parseAll("(1234 )( 2345) 3456"),
      Some(NonEmptyList.of(1234, 2345, 3456))
    )

    assertEquals(
      p.parseAll("(1234 ) ( 2345) 3456"),
      None,
      "does not accept spaces between () per construction"
    )
  }

  test("mapN") {

    case class User(name: String, age: Int, surname: String)

    val d = (alpha, digits, alpha).mapN(User.apply)

    val p: Parser[User] = parenthesis(d).orElse(d)

    assertEquals(
      p.parseAll("john1234green"),
      Some(User("john", 1234, "green"))
    )
  }
