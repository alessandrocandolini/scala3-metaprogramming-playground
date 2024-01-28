package com.alessandrocandolini.calculator

enum AstF[A,B]:
  case ValF(value : A)
  case BinaryOperationF(operation: BinaryOperation, value1: B, value2: B)
  case NegateF(value : B)

enum BinaryOperation:
   case Add
   case Multiply

object AstF:
  type Ast[A] = AstF[A,A]



