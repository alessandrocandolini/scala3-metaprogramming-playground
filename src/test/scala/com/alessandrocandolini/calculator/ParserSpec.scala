package com.alessandrocandolini.calculator

import cats.implicits.{catsSyntaxApplyOps, catsSyntaxTuple2Semigroupal}
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

  test("optional parenthesis works when parenthesis are present") {

    val p = optionalParenthesis(digits)

    assertEquals(
      p.parseAll("(1234)"),
      Some(1234)
    )
  }

  test("optional parenthesis works when parenthesis are not present") {

    val p = optionalParenthesis(digits)

    assertEquals(
      p.parseAll("1234"),
      Some(1234)
    )
  }
