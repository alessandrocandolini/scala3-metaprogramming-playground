package com.alessandrocandolini.macros

import Json.*
import JsonPattern.*
import scala.quoted.{Expr, Quotes}

// from Nicolas Stucki's presentation - Implementing a Macro

object JsonMacro:

  // Glossary of Multi-Staging
  // 1. quote: quoted blocks '(..) or '{..} contain typed code and are used to stage / delay the execution
  // 2. splice: ${..} is evaluated and the results spliced into the surrounding expression
  // (ie,  ${..} evaluates the code within the splice at compile-time and places the result in the generated code).
  // Also used to insert code into quotes
  // 3. macro: inline method starting with a splice outside of a quote, so the content of the splice needs to be evaluated at compile time when inlining

  extension (inline context: StringContext) { // inlined to avoid instantiating the context at runtime
    inline def json(inline args: Json*): Json =
      ${ jsonExpr('{ context }, '{ args }) }
  }

  def jsonExpr(contextExpr: Expr[StringContext], argsExpr: Expr[Seq[Json]])(using
    quotes: Quotes
  ): Expr[Json] =
    val context: StringContext =
      contextExpr.valueOrAbort // asks Expr for its value, needs a FromExpr instance
    val parts: Seq[String]                   = context.parts
    val pattern: Either[String, JsonPattern] = JsonPattern.parse(parts.toList).toRight("cannot parse pattern")
    pattern match
      case Left(value) =>
        '{ Null } // quotes.reflect.report.errorAndAbort(s"StringContext.json error: $value")
      case Right(value) => compile(context, value)

  def compile(context: StringContext, pattern: JsonPattern)(using Quotes): Expr[Json] = '{ Null }
