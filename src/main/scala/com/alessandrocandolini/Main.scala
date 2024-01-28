package com.alessandrocandolini

import cats.effect.{ExitCode, IO}
import com.alessandrocandolini.calculator.Program
import com.monovore.decline.Opts
import com.alessandrocandolini.cli.Args
import com.alessandrocandolini.macros.DebugMacro.printAst
import com.alessandrocandolini.utils.CommandIOAppSimple

object Main
    extends CommandIOAppSimple(
      name = "scala3-metaprogramming-playground",
      header = "scala3-metaprogramming-playground",
      version = "0.1"
    ):

  override def run: Opts[IO[ExitCode]] = Args.readArgs.map(program)

  val program: Args => IO[ExitCode] = args => Program.run(args.eval)
