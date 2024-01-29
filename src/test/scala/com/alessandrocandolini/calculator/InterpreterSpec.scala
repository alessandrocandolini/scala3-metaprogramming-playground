package com.alessandrocandolini.calculator

import cats.implicits.*
import com.alessandrocandolini.DefaultSuite
import com.alessandrocandolini.calculator.AstF.{Ast, *}
import com.alessandrocandolini.calculator.Interpreter.evaluate

import scala.language.implicitConversions

class InterpreterSpec extends DefaultSuite:

  test("evaluate simple ast") {
    val ast: Ast[Double] = (3d.literal + 4d.literal) * (2d.literal + 5d.literal) + 6d.literal
    assertEquals(
      ast.evaluate,
      Right(55.0)
    )
  }

  test("evaluate simple ast with division by 0") {
    val ast: Ast[Double] =
      (literal(3d) + literal(4d)) / ((literal(2d) * literal(3d)) - literal(5.5) - literal(0.5))
    assertEquals(
      ast.evaluate,
      Left(EvalError.CannotDivideByZero)
    )
  }
