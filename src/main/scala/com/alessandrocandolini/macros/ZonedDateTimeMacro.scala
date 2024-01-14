package com.alessandrocandolini.macros

import cats.implicits.catsSyntaxEither
import java.time.ZonedDateTime
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.util.Try
import java.time.format.DateTimeParseException
import MacroUtils.*
import MacroUtils.given

object ZonedDateTimeMacro:

  extension (inline context: StringContext) {
    inline def iso8601(inline args: Any*): ZonedDateTime =
      ${ iso8601Impl('{ context }, '{ args }) }
  }

  private def iso8601Impl(contextExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using
    q: Quotes
  ): Expr[ZonedDateTime] = {

    val context = contextExpr.valueOrAbort

    context.asSingleStringOrFail match
      case Some(s) => iso8601ParseImpl(Expr(s))
      case None    => fail("string interpolation unsupported", argsExpr)
  }

  private def iso8601ParseImpl(stringExpr: Expr[String])(using
    q: Quotes
  ): Expr[ZonedDateTime] = {
    val s = stringExpr.valueOrAbort
    parse(s) match
      case Left(message)   => fail(message)
      case Right(dateTime) => Expr(dateTime)
  }

  private def parse(s: String): Either[String, ZonedDateTime] =
    Try {
      ZonedDateTime.parse(s)
    }.toEither.leftMap(prettyErrorMessage(s, _))

  private def prettyErrorMessage(s: String, t: Throwable): String = {
    val position    = Some(t).collect { case e: DateTimeParseException =>
      e.getErrorIndex
    }
    val explanation =
      position.fold(t.getMessage)(value => (" " * value) + "^" + s"\nError at position $value")
    s"$s cannot be parsed as ISO-8601 date format.\n$explanation"
  }
