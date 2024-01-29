package com.alessandrocandolini.calculator

import cats.implicits.*
import com.alessandrocandolini.DefaultSuite
import com.alessandrocandolini.calculator.GrammarParser.*
import com.alessandrocandolini.calculator.AstF.{Ast, *}

import scala.language.implicitConversions

class GrammarParserSpec extends DefaultSuite:

  test("can parse value") {
    assertEquals(
      parseAst("14.4"),
      Some(literal(14.4))
    )
  }

  test("can parse simple expression with parenthesis and +") {
    val expected: Ast[Double] = literal(14d) + literal(7.1)
    assertEquals(
      parseAst("(14+7.1)"),
      Some(expected)
    )
  }

  test("can parse simple expression with parenthesis and *") {
    val expected: Ast[Double] = literal(14d) * literal(7.1)
    assertEquals(
      parseAst("(14*7.1)"),
      Some(expected)
    )
  }

  test("can parse simple expression with parenthesis and -") {
    val expected: Ast[Double] = literal(14d) - literal(7.1)
    assertEquals(
      parseAst("(14-7.1)"),
      Some(expected)
    )
  }

  test("can parse simple expression with parenthesis and /") {
    val expected: Ast[Double] = literal(14d) / literal(7.1)
    assertEquals(
      parseAst("(14/7.1)"),
      Some(expected)
    )
  }

  test("can parse nested expression with parenthesis, to the left") {
    val expected: Ast[Double] =
      literal(4.0) + (literal(2.0) * literal(3.0))
    assertEquals(
      parseAst("(4+(2*3))"),
      Some(expected)
    )
  }

  test("can parse nested expression with parenthesis, to the right") {
    val expected: Ast[Double] =
      (literal(4.0) + literal(2.0)) * literal(3.0)
    assertEquals(
      parseAst("((4+2)*3)"),
      Some(expected)
    )
  }

  test("can parse nested expression with parenthesis, to the left and to the right") {
    val expected: Ast[Double] =
      (literal(4.0) + literal(2.0)) * (literal(3.5) * literal(1.0))
    assertEquals(
      parseAst("((4+2)*(3.5*1))"),
      Some(expected)
    )
  }

  test("can parse double nested expression with parenthesis, to the left and to the right") {
    val expected: Ast[Double] =
      (literal(4.0) + (literal(2.0) - literal(5.1))) / (literal(3.5) * literal(1.0))
    assertEquals(
      parseAst("((4+(2-5.1))/(3.5*1))"),
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