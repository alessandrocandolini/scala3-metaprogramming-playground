package com.alessandrocandolini.calculator

import com.alessandrocandolini.calculator.AstMacro.eval

import scala.compiletime.ops.double.*

object AstExample:

  val example1: 14d = eval"2*(3+4)"

  val example2: 21.48d = eval"(14.37+7.11)"

  val example3: 2d = eval"2.0"

  val example4: 1.8d = eval"((3.6*1.5)/(5.1+(2-4.1)))"

  val example5: 11.2d = eval"(1+2)*3-(4/5-3)"
