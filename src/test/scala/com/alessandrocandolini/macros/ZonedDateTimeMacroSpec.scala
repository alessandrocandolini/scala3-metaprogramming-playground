package com.alessandrocandolini.macros

import munit.FunSuite

class ZonedDateTimeMacroSpec extends FunSuite:

  test("does not compile invalid date, the compilation error includes error position") {

    val expectedError = """|error:
                            |2021-02-3T10:15:30Z cannot be parsed as ISO-8601 date format.
                            |        ^
                            |Error at position 8
                            |""".stripMargin

    assertNoDiff(
      compileErrors("""
           import ZonedDateTimeMacro.iso8601
           import java.time.ZonedDateTime
           val doesNotCompile : ZonedDateTime = iso8601"2021-02-3T10:15:30Z"
           val compiles : ZonedDateTime = iso8601"2021-02-03T10:15:30Z"
           """).take(expectedError.length),
      expectedError
    )
  }
