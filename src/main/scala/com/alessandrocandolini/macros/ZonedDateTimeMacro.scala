package com.alessandrocandolini.macros

import cats.implicits.catsSyntaxEither
import java.time.ZonedDateTime
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.util.Try
import java.time.format.DateTimeParseException

object ZonedDateTimeMacro:

  extension (inline context: StringContext)
    inline def date(inline args: Any*): ZonedDateTime =
      ${ dateImpl('{ context }, '{ args }) }

  private def dateImpl(contextExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using
    q: Quotes
  ): Expr[ZonedDateTime] = {
    val context = contextExpr.valueOrAbort

    context.parts.toList match
      case s :: Nil =>
        parse(s.trim()) match
          case Left(message) => q.reflect.report.errorAndAbort(message)
          case Right(value)  => '{ parse(${ Expr(s.trim()) }).toOption.get }

      case _ => fail(s"interpolation not supported", argsExpr)
  }

  private def fail[A, B](message: String, expr: Expr[A])(using q: Quotes): Expr[B] =
    q.reflect.report.errorAndAbort(message, expr)

  def parse(s: String): Either[String, ZonedDateTime] =
    Try {
      ZonedDateTime.parse(s)
    }.toEither.leftMap(errorMessage(s, _))

  def errorMessage(s: String, e: Throwable): String = {
    val position  = Some(e).collect { case ex: DateTimeParseException =>
      ex.getErrorIndex()
    }
    val extraInfo = position match
      case Some(value) => "\n" + (" " * value) + "^" + s"\nError at position $value"
      case None        => s"\nError: ${e.getMessage}"

    s"$s cannot be parsed as ISO-8601 date format" + extraInfo
  }
