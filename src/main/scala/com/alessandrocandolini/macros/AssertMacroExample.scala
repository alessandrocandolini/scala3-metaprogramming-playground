package com.alessandrocandolini.macros

import com.alessandrocandolini.macros.AssertMacro.assertEquals

object AssertMacroExample:
  assertEquals(1 + 2, 3)

  assertEquals(3 + 2, 5)

  private inline def x = (1 + 2) * 3

  assertEquals(x, 9)
