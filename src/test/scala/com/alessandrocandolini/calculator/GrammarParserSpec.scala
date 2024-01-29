package com.alessandrocandolini.calculator

import cats.implicits.*
import com.alessandrocandolini.DefaultSuite
import com.alessandrocandolini.calculator.GrammarParser.{literalP, operationParser, parseAst, realParser}
import com.alessandrocandolini.calculator.AstF.{Ast, *}

import scala.language.implicitConversions

class GrammarParserSpec extends DefaultSuite:

  test("can parse value") {
    assertEquals(
      parseAst("14.4"),
      Some(literal(14.4))
    )
  }

  test("can parse simple expression with parenthesis") {
    val expected: Ast[Double] = literal(14d) + literal(7.1)
    assertEquals(
      parseAst("(14+7.1)"),
      Some(expected)
    )
  }

  test("can parse simple expression without parenthesis".ignore) {
    val expected: Ast[Double] = literal(14d) + literal(7.1)
    assertEquals(
      parseAst("14+7.1"),
      Some(expected)
    )
  }

  test("can parse nested expression with parenthesis".ignore) {
    val expected: Ast[Double] =
      literal(4.0) + (literal(2.0) * literal(3.0))
    assertEquals(
      parseAst("(4+(2*3))"),
      Some(expected)
    )
  }

//  test("experiment3") {
//    assertEquals(
//      p.parseAll("2"),
//      Some(literal(2))
//    )
//  }
