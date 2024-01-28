package com.alessandrocandolini.calculator

import cats.implicits.*
import com.alessandrocandolini.DefaultSuite
import com.alessandrocandolini.calculator.AstF.{Ast, *}
import com.alessandrocandolini.calculator.Interpreter.evaluate

import scala.language.implicitConversions

class InterpreterSpec extends DefaultSuite:

  test("evaluate simple ast") {
    val ast : Ast[Int] = (literal(3) + literal(4)) * (literal(2) + literal(5)) + literal(6)
    assertEquals(
      ast.evaluate,
      Right(55)
    )
  }
