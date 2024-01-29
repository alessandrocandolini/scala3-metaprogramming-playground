package com.alessandrocandolini.calculator

import cats.implicits.catsSyntaxEither
import com.alessandrocandolini.calculator.AstF.Ast
import com.alessandrocandolini.calculator.Interpreter.evaluate
import com.alessandrocandolini.macros.MacroUtils.{asSingleStringOrFail, fail}

import scala.quoted.{Expr, Quotes, ToExpr}

private enum AstMacroError:
  case UnsupportedStringInterpolation
  case EvaluationError(e: EvalError)
  case ParsingError(expression: String)

object AstMacroError:
  extension (e: AstMacroError) {
    def message: String = e match
      case UnsupportedStringInterpolation                => "string interpolation unsupported"
      case EvaluationError(EvalError.CannotDivideByZero) => "division by zero is not defined"
      case ParsingError(s)                               => s"error parsing expression $s"
  }

object AstMacro:

  extension (inline context: StringContext) {
    transparent inline def eval(inline args: Any*): Double =
      ${ evalImpl('{ context }, '{ args }) }
  }

  private def evalImpl(stringContextExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using
    quotes: Quotes
  ): Expr[Double] = {
    val stringContext = stringContextExpr.valueOrAbort

    val r: Either[AstMacroError, Double] = for {
      s   <- stringContext.asSingleStringOrFail.toRight(AstMacroError.UnsupportedStringInterpolation)
      ast <- GrammarParser.parseAst(s).toRight(AstMacroError.ParsingError(s))
      res <- ast.evaluate.leftMap(AstMacroError.EvaluationError.apply)
    } yield res

    r match {
      case Left(e)      => fail(e.message)
      case Right(value) => Expr(value)
    }
  }
