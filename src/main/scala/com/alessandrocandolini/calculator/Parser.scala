package com.alessandrocandolini.calculator

import cats.implicits.*
import cats.data.{NonEmptyList, NonEmptySet, StateT}
import cats.FunctorFilter

type Parser[A] = StateT[Option, String, A]

object Parser:

  extension [A](p: Parser[A]) {
    def parse(s: String): Option[(String, A)] =
      p.run(s)

    def parseAll(s: String): Option[A] =
      parse(s).filter(_._1.isEmpty).map(_._2)

    def orElse(p2: Parser[A]): Parser[A] =
      p.combineK(p2)
  }

  def choose[A](one: Parser[A], other: Parser[A]*): Parser[A] =
    NonEmptyList.fromList(other.toList) match {
      case Some(ps) => ps.foldLeft(one) { case (p1, p2) => p1.orElse(p2) }
      case None     => one
    }

  def anyChar: Parser[Char] = StateT { s =>
    s.headOption.map { c =>
      s.tail -> c
    }
  }

  def char(c: Char): Parser[Char] = anyChar.filter(c1 => c1 == c)

  def string(s: String): Parser[String] = s.toList.traverse(char).map(_.toString())

  def boolean: Parser[Boolean] =
    string("true").as(true).orElse(string("false").as(false))

  def booleanCaseInsensitive: Parser[Boolean] =
    choose(string("true"), string("True")).as(true).orElse(choose(string("false"), string("False")).as(false))

  val openBrace: Parser[Unit]  = char('(').void
  val closeBrace: Parser[Unit] = char(')').void
  val newline: Parser[Unit]    = char('\n').void
  val space: Parser[Unit]      = char(' ').void
//  val spaces : Parser[Unit] = repeat0(space)

  def parenthesis[A](parser: Parser[A]): Parser[A] =
    openBrace *> parser <* closeBrace
