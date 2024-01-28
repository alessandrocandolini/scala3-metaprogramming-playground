package com.alessandrocandolini.calculator

import com.alessandrocandolini.calculator.AstF.Ast

enum EvalError:
  case DivisionByZero

object EvalError:
  extension (e: EvalError) {
    def message: String = e match
      case EvalError.DivisionByZero => "cannot divide by 0"
  }

object Interpreter:

  extension (input: Ast[Int]) {
    def evaluate: Either[EvalError, Int] = Right(14)
  }
