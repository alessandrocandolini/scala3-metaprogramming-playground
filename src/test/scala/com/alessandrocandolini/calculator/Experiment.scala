package com.alessandrocandolini.calculator

import cats.data.NonEmptyList
import cats.implicits.*
import com.alessandrocandolini.DefaultSuite
import com.alessandrocandolini.calculator.AstF.{Ast, binaryOperation, literal}
import com.alessandrocandolini.calculator.GrammarParser.*
import com.alessandrocandolini.calculator.Parser.*

class Experiment extends DefaultSuite:

  val binaryOperation2P: Parser[Ast[Double]] =
    (literalP, choose(lowPriorityOperation, highPriorityOperation), literalP).mapN { case (v1, op, v2) =>
      binaryOperation(op, v1, v2)
    }

  val p: Parser[Ast[Double]] = parenthesis(binaryOperation2P).orElse(binaryOperation2P).orElse(literalP)

  val expected: Ast[Double] = literal(2.0) * literal(3.0)

  test("experiment1") {

    assertEquals(
      p.parseAll("(2*3)"),
      Some(expected)
    )
  }

  test("experiment2") {
    assertEquals(
      p.parseAll("2*3"),
      Some(expected)
    )
  }

  test("experiment3") {
    assertEquals(
      p.parseAll("(2*3"), // unbalance
      None
    )
  }
