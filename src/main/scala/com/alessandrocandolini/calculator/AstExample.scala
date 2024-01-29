package com.alessandrocandolini.calculator

import com.alessandrocandolini.calculator.AstMacro.eval

import scala.compiletime.ops.double.*

object AstExample:

  val example1: 14d = eval"2*(3+4)"

  val example2: 21.48d = eval"(14.37+7.11)"

  val example3: 2d = eval"2.0"
