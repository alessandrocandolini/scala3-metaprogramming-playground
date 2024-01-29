package com.alessandrocandolini.calculator

import com.alessandrocandolini.calculator.AstF.*
import com.alessandrocandolini.calculator.BinaryOperation.*
import higherkindness.droste.{GAlgebra, GAlgebraM, GTrans, scheme}

enum EvalError:
  case CannotDivideByZero

object Interpreter:

  extension (input: Ast[Double]) {
    def evaluate: Either[EvalError, Double] =
      scheme.cataM(evalGAlgebraM).apply(input)
  }

  private def evalGAlgebraM: GAlgebraM[Either[EvalError, _], AstF[Double, _], Double, Double] = GAlgebraM {
    case LiteralF(value)                            => Right(value)
    case BinaryOperationF(Add, value1, value2)      => Right(value1 + value2)
    case BinaryOperationF(Multiply, value1, value2) => Right(value1 * value2)
    case BinaryOperationF(Subtract, value1, value2) => Right(value1 - value2)
    case BinaryOperationF(Divide, _, 0d)            => Left(EvalError.CannotDivideByZero)
    case BinaryOperationF(Divide, value1, value2)   => Right(value1 / value2)
    case NegateF(value)                             => Right(-value)
  }
