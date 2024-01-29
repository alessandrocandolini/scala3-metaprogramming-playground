package com.alessandrocandolini.calculator

import com.alessandrocandolini.calculator.AstF.Ast
import com.alessandrocandolini.calculator.Interpreter.evaluate
import com.alessandrocandolini.macros.MacroUtils.{asSingleStringOrFail, fail}

import scala.quoted.{Expr, Quotes, ToExpr}

object AstMacro:

  extension (inline context: StringContext) {
    inline def ast(inline args: Any*): Ast[Double] =
      ${ astImpl('{ context }, '{ args }) }

    inline def eval(inline args: Any*): Double =
      ${ evalImpl('{ context }, '{ args }) }
  }

  private def astImpl(stringContextExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using
    quotes: Quotes
  ): Expr[Ast[Double]] = {
    val stringContext = stringContextExpr.valueOrAbort

    stringContext.asSingleStringOrFail match {
      case Some(s) =>
        GrammarParser.parseAst(s) match
          case Some(_) => '{ GrammarParser.parseAst(${ Expr(s) }).get } // need to find a way to ave ExprrTo
          case None    => fail(s"error parsing $s")
      case None    => fail("string interpolation unsupported", argsExpr)
    }
  }

  private def evalImpl(stringContextExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using
    quotes: Quotes
  ): Expr[Double] = {
    val stringContext = stringContextExpr.valueOrAbort

    stringContext.asSingleStringOrFail match {
      case Some(s) =>
        GrammarParser.parseAst(s) match
          case Some(ast) =>
            ast.evaluate match
              case Left(EvalError.CannotDivideByZero) => fail("division by zero not defined")
              case Right(value)                       => Expr(value)
          case None      => fail(s"error parsing $s")
      case None    => fail("string interpolation unsupported", argsExpr)
    }
  }
