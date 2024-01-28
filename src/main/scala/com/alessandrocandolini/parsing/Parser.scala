package com.alessandrocandolini.parsing

import cats.implicits.*
import cats.Applicative
import cats.arrow.FunctionK
import cats.data.StateT

enum Parser[+A]:
  case Pure(a: A)
  case CharParser(c : Char)
  case CharCaseInsensitiveParser(c : Char)
  case Digit
  case OrElse(p1 : Parser[A], p2 : Parser[A])

object Parser:

  type UnderlyingParser[A] = StateT[Option, String, A]

  private def anyChar: UnderlyingParser[Char] = StateT { s =>
    s.headOption.map { c =>
      s.tail -> c
    }
  }

