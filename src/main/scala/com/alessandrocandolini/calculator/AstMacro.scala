package com.alessandrocandolini.calculator

import com.alessandrocandolini.calculator.AstF.Ast
import com.alessandrocandolini.calculator.Interpreter.evaluate
import com.alessandrocandolini.macros.MacroUtils.{asSingleStringOrFail, fail}

import scala.quoted.{Expr, Quotes, ToExpr}

object AstMacro:

  extension (inline context: StringContext) {
    inline def ast(inline args: Any*): Ast[Int] =
      ${ astImpl('{ context }, '{ args }) }

    inline def eval(inline args: Any*): Int =
      ${ evalImpl('{ context }, '{ args }) }
  }

  private def astImpl(stringContextExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using
    quotes: Quotes
  ): Expr[Ast[Int]] = {
    val stringContext = stringContextExpr.valueOrAbort

    stringContext.asSingleStringOrFail match {
      case Some(s) =>
        GrammarParser.parseAll(s) match
          case Some(_) => '{ GrammarParser.parseAll(${ Expr(s) }).get } // need to find a way to ave ExprrTo
          case None    => fail(s"error parsing $s")
      case None    => fail("string interpolation unsupported", argsExpr)
    }
  }

  private def evalImpl(stringContextExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using
    quotes: Quotes
  ): Expr[Int] = {
    val stringContext = stringContextExpr.valueOrAbort

    stringContext.asSingleStringOrFail match {
      case Some(s) =>
        GrammarParser.parseAll(s) match
          case Some(ast) =>
            ast.evaluate match
              case Left(value)  => fail(s"error interpreting $s: ${value.message}")
              case Right(value) => Expr(value)
          case None      => fail(s"error parsing $s")
      case None    => fail("string interpolation unsupported", argsExpr)
    }
  }
