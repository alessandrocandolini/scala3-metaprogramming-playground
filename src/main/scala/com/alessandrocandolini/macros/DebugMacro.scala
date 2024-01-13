package com.alessandrocandolini.macros

import scala.quoted.{Expr, Quotes, Type, quotes}

object DebugMacro:
  inline def printAst[A](inline a: A) = ${ printAstImpl('{a}) }

  def printAstImpl[A: Type](expr: Expr[A])(using Quotes): Expr[A] = {
    import quotes.reflect.*
    val term = expr.asTerm
    println(s"===========Tree of type ${Type.show}=========:")
    println(term.show(using Printer.TreeAnsiCode))
    println("===========================")
    println(term.show(using Printer.TreeStructure))
    println("===========================")
    expr
  }