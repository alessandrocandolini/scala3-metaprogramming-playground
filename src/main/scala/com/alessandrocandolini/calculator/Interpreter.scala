package com.alessandrocandolini.calculator

import com.alessandrocandolini.calculator.AstF.*
import com.alessandrocandolini.calculator.BinaryOperation.*
import higherkindness.droste.{GAlgebraM, scheme}

enum EvalError:
  case CannotDivideByZero

object Interpreter:

  extension (input: Ast[Int]) {
    def evaluate: Either[EvalError, Int] =
      scheme.cataM(gAlgebraM).apply(input)
  }

  def gAlgebraM: GAlgebraM[[B] =>> Either[EvalError, B], [B] =>> AstF[Int, B], Int, Int] = GAlgebraM {
    case LiteralF(value)                            => Right(value)
    case BinaryOperationF(Add, value1, value2)      => Right(value1 + value2)
    case BinaryOperationF(Multiply, value1, value2) => Right(value1 * value2)
    case BinaryOperationF(Subtract, value1, value2) => Right(value1 - value2)
    case BinaryOperationF(Divide, value1, value2)   =>
      if (value2 != 0) {
        Right(value1 / value2)
      } else {
        Left(EvalError.CannotDivideByZero)
      }
    case NegateF(value)                             => Right(-value)
  }
