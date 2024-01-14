package com.alessandrocandolini.macros

import scala.quoted.{Expr, Quotes, Type, quotes}

object DebugMacro:
  inline def printAst[A](inline a: A) =
    ${ printAstImpl('{ a }) }

  def printAstImpl[A: Type](expr: Expr[A])(using Quotes): Expr[A] = {
    import quotes.reflect.*
    val term = expr.asTerm

    val terminalWidth     = 120
    val separatorLine     = "\n" + ("=" * terminalWidth) + "\n"
    val title             = s"Tree of type ${Type.show}"
    val padding           = (terminalWidth - title.length - 2) / 2
    val centeredTitleLine = "\n" + ("=" * padding) + " " + title + " " + ("=" * padding) + "\n"

    println(centeredTitleLine)
    println(term.show(using Printer.TreeAnsiCode))
    println(separatorLine)
    println(term.show(using Printer.TreeStructure))
    println(separatorLine)

    expr
  }
