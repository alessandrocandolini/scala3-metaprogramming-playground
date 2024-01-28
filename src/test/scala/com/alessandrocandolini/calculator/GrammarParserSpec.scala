package com.alessandrocandolini.calculator

import cats.implicits.{catsSyntaxApplyOps, catsSyntaxTuple2Semigroupal}
import com.alessandrocandolini.DefaultSuite
import com.alessandrocandolini.calculator.GrammarParser.{literalP, parseAst}
import com.alessandrocandolini.calculator.AstF.*

import scala.language.implicitConversions

class GrammarParserSpec extends DefaultSuite:

  test("can parse value") {
    assertEquals(
      parseAst("14"),
      Some(literal(14))
    )
  }

  test("can parse simple expression with parenthesis") {
    val expected : Ast[Int] = literal(14) + literal(7)
    assertEquals(
      parseAst("(14+7)"),
      Some(expected)
    )
  }

  test("can parse simple expression without parenthesis".ignore) {
    val expected : Ast[Int] = literal(14) + literal(7)
    assertEquals(
      parseAst("14+7"),
      Some(expected)
    )
  }

  test("can parse nested expression with parenthesis".ignore) {
    val expected: Ast[Int] =
      literal(2) * (literal(3) + literal(4))
    assertEquals(
      parseAst("(2*3)+4"),
      Some(expected)
    )
  }

