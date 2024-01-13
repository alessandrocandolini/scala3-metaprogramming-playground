package com.alessandrocandolini.macros

import Json.*
import JsonPattern.*
import scala.quoted.{Expr, Quotes}

// from Nicolas Stucki's presentation - Implementing a Macro

object JsonMacro:

  // Glossary of Multi-Staging
  // quote: '(..) or '{..}  used to stage / delay the execution
  // splice: ${..} to evaluate , or to insert code into quotes
  // macro: inline method starting with a splice outside of a quote, so the content of the splice needs to be evaluated at compile time when inlining

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
      case Left(value)  => quotes.reflect.report.errorAndAbort(s"StringContext.json error: $value")
      case Right(value) => compile(context, value)

  def compile(context: StringContext, pattern: JsonPattern)(using Quotes): Expr[Json] =
    pattern match
      case StringPattern(value)       => Expr(value)
      case ArrayPattern(values @ _*)  => ???
      case ObjectPattern(values @ _*) => ???
      case InterpolatedValue          => ???
