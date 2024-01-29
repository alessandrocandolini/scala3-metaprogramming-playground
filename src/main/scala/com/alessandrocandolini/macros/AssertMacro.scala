package com.alessandrocandolini.macros

import scala.compiletime.summonInline
import scala.quoted.{Expr, FromExpr, Quotes}

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
      quotes.reflect.report.errorAndAbort(
        s"values are not the same: expected $expected but got $obtained instead."
      )
    }
  }

  inline def assertDoubleEquals(
    inline obtained: Double,
    inline expected: Double
  ): Unit =
    ${ assertDoubleEqualsImpl('{ obtained }, '{ expected }) }

  private def assertDoubleEqualsImpl(
    obtainedExpr: Expr[Double],
    expectedExpr: Expr[Double]
  )(using quotes: Quotes): Expr[Unit] = {

    val obtained = obtainedExpr.valueOrAbort
    val expected = expectedExpr.valueOrAbort

    if (obtained == expected) {
      '{ () }
    } else {
      quotes.reflect.report.errorAndAbort(
        s"values are not the same: expected $expected but got $obtained instead."
      )
    }
  }
