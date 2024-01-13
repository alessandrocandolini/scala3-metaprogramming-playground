package com.alessandrocandolini.macros

import Json.*

type Json = JsObject | JsArray | String | Null

object Json:

  given CanEqual[Json, Json] = CanEqual.derived

  type Null = Null.type
  object Null

  case class JsArray(values: Json*)

  case class JsObject(values: Map[String, Json]) extends Selectable:
    def selectDynamic(name: String): Json | Null = values.getOrElse(name, Null)
    // specifically  not used an Option here because it does not play well with refinement types
