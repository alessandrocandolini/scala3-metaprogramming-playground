package com.alessandrocandolini.macros

import scala.quoted.Expr

// from Nicolas Stucki's presentation - Implementing a Macro

type Json = JsObject | JsArray | String | Null

type Null = Null.type
object Null

type Undefined = Undefined.type
object Undefined

case class JsArray(values: Json*)

case class JsObject(values: Map[String, Json]) extends Selectable:
  def selectDynamic(name: String) : Json | Undefined =
    values.getOrElse(name, Undefined)
    // specifically  not used an Option here because it does not play well with refinement types

object Json:

  // Glossary of Multi-Staging
  // quote: '(..) or '{..}  used to stage / delay the execution
  // splice: ${..} to evaluate , or to insert code into quotes
  // macro: inline method starting with a splice outside of a quote, so the content of the splice needs to be evaluated at compile time when inlining

  extension (inline context : StringContext) { // inlined to avoid instantiating the context at runtime
    inline def json(inline args : Json*): Json =
      ${ jsonExpr('{context}, '{args}) }
  }

  def jsonExpr(context: Expr[StringContext], args: Expr[Seq[Json]]) : Expr[Json] = ???


object Examples:

  import Json.json

  val example1: Json =
    json"""
          | {
          |  "name": "hello world",
          |  "speaker": "Guglielmo Marconi"
          | }
          |"""

  val example2: Json =
    json"""
          | {
          |  "name": "hello world2",
          |  "speaker": "Leonardo da Vinci"
          | }
          |"""

  val example3: Json =
    json"""
          | [
          |   $example1,
          |   $example2
          | ]
          |"""