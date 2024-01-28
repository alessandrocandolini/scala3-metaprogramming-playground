package com.alessandrocandolini.calculator

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
