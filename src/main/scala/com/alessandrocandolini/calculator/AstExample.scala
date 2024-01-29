package com.alessandrocandolini.calculator

import com.alessandrocandolini.calculator.AstMacro.eval
import com.alessandrocandolini.macros.AssertMacro.assertDoubleEquals

object AstExample:

  assertDoubleEquals(eval"2*(3+4)", 14d)
