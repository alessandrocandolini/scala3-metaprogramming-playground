package com.alessandrocandolini.macros

import AssertMacro.assertEquals
import JsonMacro.json
import Json.*

object JsonExamples:

  val example1: Json =
    json"""
          | {
          |  "name": "hello world",
          |  "speaker": "Guglielmo Marconi"
          | }
          |"""

  val expected1: Json = JsObject(Map(
    "name" -> "hello world",
    "speaker" -> "Guglielmo Marconi"
  ))

  val example2: Json =
    json"""
          | {
          |  "name": "hello world2",
          |  "speaker": "Leonardo da Vinci"
          | }
          |"""

  val expected2: Json = JsObject(Map(
    "name" -> "hello world2",
    "speaker" -> "Leonardo da Vinci"
  ))

  val example3: Json =
    json"""
          | [
          |   $example1,
          |   $example2
          | ]
          |"""

  val expected3: Json = JsArray(expected1, expected2)

  assertEquals(example1,  expected1)

  assertEquals(example3,  expected3)
