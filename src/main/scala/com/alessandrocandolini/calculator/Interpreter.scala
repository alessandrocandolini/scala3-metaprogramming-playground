package com.alessandrocandolini.calculator

import com.alessandrocandolini.calculator.AstF.Ast

enum EvalError:
  case DivisionByZero

object Interpreter:

  def eval(input: Ast[Int]): Either[EvalError, Int] = ???

