package com.alessandrocandolini.calculator

import com.alessandrocandolini.calculator.AstMacro.eval
import com.alessandrocandolini.macros.AssertMacro.assertEquals

object AstExample:

  assertEquals(eval"2*(3+4)", 14)
