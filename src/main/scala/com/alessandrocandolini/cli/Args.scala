package com.alessandrocandolini.cli

import com.monovore.decline.Opts
import cats.implicits.*

enum Verbose derives CanEqual:
  case Verbose
  case Quiet

case class Args(
  eval: String,
  verbose: Verbose
) derives CanEqual

object Args:

  val readArgs: Opts[Args] = (
    Opts
      .option[String]("eval", short = "e", help = "string to evaluate")
      .validate("eval arg must be nonempty")(_.nonEmpty),
    Opts
      .flag("verbose", help = "Verbose output")
      .map(_ => Verbose.Verbose)
      .withDefault(Verbose.Quiet)
  ).mapN(Args.apply)
