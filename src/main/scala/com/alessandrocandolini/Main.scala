package com.alessandrocandolini

import cats.effect.IO
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

  override def run: Opts[IO[Unit]] = Args.readArgs.map(program)

  inline def sayHello(name: String): String = s"hello, $name"

  val program: Args => IO[Unit] = args =>
    val s = printAst {
      sayHello("world!!")
    }
    IO.println(s"$s $args")

  def getting =
    println("hello")
