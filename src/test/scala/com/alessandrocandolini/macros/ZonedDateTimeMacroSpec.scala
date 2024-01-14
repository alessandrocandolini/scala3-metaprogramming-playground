package com.alessandrocandolini.macros

import munit.FunSuite

class ZonedDateTimeMacroSpec extends FunSuite:

  test("does not compile invalid date, the compilation error includes error position") {
    assertNoDiff(
      compileErrors(
        """
           import ZonedDateTimeMacro.date

           val example2 = date"2021-02-3T10:15:30Z"
        """),
      """|error:
         |2021-02-3T10:15:30Z cannot be parsed as ISO-8601 date format
         |        ^
         |Error at position 8
         |      compileErrors(
         |                  ^
         |""".stripMargin
    )
  }
