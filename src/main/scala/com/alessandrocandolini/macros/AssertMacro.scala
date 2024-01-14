package com.alessandrocandolini.macros

import scala.compiletime.summonInline
import scala.quoted.{Expr, FromExpr, Quotes, Type}

object AssertMacro:
  inline def assertEquals(
    inline obtained: Int,
    inline expected: Int
  ): Unit =
    ${ assertEqualsImpl('{ obtained }, '{ expected }) }

  private def assertEqualsImpl(
    obtainedExpr: Expr[Int],
    expectedExpr: Expr[Int]
  )(using quotes: Quotes): Expr[Unit] = {

    val obtained = obtainedExpr.valueOrAbort
    val expected = expectedExpr.valueOrAbort

    if (obtained == expected) {
      '{ () }
    } else {
      quotes.reflect.report.errorAndAbort(s"values are not the same: expected $expected but got $obtained instead.")
    }
  }
