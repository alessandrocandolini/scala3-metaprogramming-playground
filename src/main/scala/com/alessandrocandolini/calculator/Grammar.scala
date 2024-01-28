package com.alessandrocandolini.calculator

import cats.{Foldable, Functor, Traverse}
import higherkindness.droste.data.Fix
import cats.derived.*

import scala.annotation.targetName

enum AstF[A, B] derives Functor, Foldable, Traverse:
  case LiteralF(value: A)
  case BinaryOperationF(operation: BinaryOperation, value1: B, value2: B)
  case NegateF(value: B)

enum BinaryOperation:
  case Add
  case Multiply
  case Subtract
  case Divide

object AstF:

  type Ast[A] = Fix[AstF[A, _]]

  extension [A](value: A) {
    def literal: Ast[A] = Fix(LiteralF(value))
  }

  def binaryOperation[A](operation: BinaryOperation, value1: Ast[A], value2: Ast[A]): Ast[A] =
    Fix(BinaryOperationF(operation, value1, value2))

  def negate[A](value: Ast[A]): Ast[A] =
    Fix(NegateF(value))

  given autowrap[A]: Conversion[A, Ast[A]] with
    def apply(a: A): Ast[A] = literal(a)

  extension [A](value1: Ast[A]) {
    @targetName("add")
    def +(value2: Ast[A]): Ast[A] = binaryOperation(BinaryOperation.Add, value1, value2)
    @targetName("multiply")
    def *(value2: Ast[A]): Ast[A] = binaryOperation(BinaryOperation.Multiply, value1, value2)
    @targetName("subtract")
    def -(value2: Ast[A]): Ast[A] = binaryOperation(BinaryOperation.Subtract, value1, value2)
    @targetName("divide")
    def /(value2: Ast[A]): Ast[A] = binaryOperation(BinaryOperation.Divide, value1, value2)
  }
