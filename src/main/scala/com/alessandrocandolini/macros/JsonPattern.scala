package com.alessandrocandolini.macros

enum JsonPattern derives CanEqual:
  case StringPattern(value : String)
  case ArrayPattern(values: JsonPattern*)
  case ObjectPattern(values: (String, JsonPattern)*)
  case InterpolatedValue

object JsonPattern:

  def parse(ss : List[String]): Option[JsonPattern] = None


