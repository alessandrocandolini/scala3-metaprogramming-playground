package com.alessandrocandolini.cli

import munit.FunSuite
import com.alessandrocandolini.cli.*
import com.alessandrocandolini.cli.Args.readArgs
import com.monovore.decline.*

class ArgsSpec extends FunSuite:

  val command = Command(
    name = "test",
    header = "test"
  ) {
    readArgs
  }

  test("opts can parse valid args with stage env") {
    val actual   = command.parse(
      Seq(
        "-e",
        "something",
        "--verbose"
      )
    )
    val expected = Args(
      eval = "something",
      verbose = Verbose.Verbose
    )

    assertEquals(actual, Right(expected))
  }
