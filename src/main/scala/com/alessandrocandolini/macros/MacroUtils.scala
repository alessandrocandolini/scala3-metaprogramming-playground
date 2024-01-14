package com.alessandrocandolini.macros

import java.time.{ZoneId, ZonedDateTime}
import scala.quoted.{Expr, Quotes, ToExpr}

object MacroUtils:

  given ToExpr[ZoneId] with
    def apply(expr: ZoneId)(using Quotes): Expr[ZoneId] =
      val zoneIdString = Expr(expr.getId)
      '{ ZoneId.of($zoneIdString) }

  given ToExpr[ZonedDateTime] with
    def apply(dateTime: ZonedDateTime)(using Quotes): Expr[ZonedDateTime] =
      val year = Expr(dateTime.getYear)
      val month = Expr(dateTime.getMonthValue)
      val day = Expr(dateTime.getDayOfMonth)
      val hour = Expr(dateTime.getHour)
      val minute = Expr(dateTime.getMinute)
      val second = Expr(dateTime.getSecond)
      val nano = Expr(dateTime.getNano)
      val zoneId = Expr(dateTime.getZone)

      '{ ZonedDateTime.of($year, $month, $day, $hour, $minute, $second, $nano, $zoneId) }

  extension (s : StringContext) {
    def asSingleStringOrFail: Option[String] = s.parts.toList match
      case ::(head, Nil) => Some(head.trim)
      case _ => None
  }

  def fail[A](message : String)(using q : Quotes): Expr[A] =
    q.reflect.report.errorAndAbort(message)

  def fail[A,B](message : String, errorExpr: Expr[A])(using q : Quotes): Expr[B] =
    q.reflect.report.errorAndAbort(message, errorExpr)

