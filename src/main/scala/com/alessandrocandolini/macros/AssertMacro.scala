package com.alessandrocandolini.macros

import scala.quoted.{Expr, FromExpr, Quotes}

object AssertMacro:

  inline def assertEquals[A, B](
                              inline obtained: A,
                              inline expected: B,
                              inline clue: => String = "values are not the same")(using CanEqual[A,B], FromExpr[A], FromExpr[B]): Unit =
    ${  assertEqualsImpl('{obtained}, '{expected}, '{clue})  }


  def assertEqualsImpl[A,B](
                             obtained: Expr[A],
                             expected: Expr[B],
                             clue: Expr[String]
                           )(using quotes: Quotes, canEqual: CanEqual[A,B], fromExprA: FromExpr[A], fromExprB: FromExpr[B]): Expr[Unit] = {
    val a = obtained.valueOrAbort
    val b = expected.valueOrAbort
    if (a == b) {
      '{ () }
    } else {
      val c = clue.valueOrAbort
      quotes.reflect.report.errorAndAbort(s": $c")
    }
  }
