package com.alessandrocandolini.calculator

import cats.Applicative
import cats.data.{NonEmptyList, StateT}
import cats.implicits.*

import scala.annotation.targetName

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

  private def pure[A](a: A): Parser[A] = Applicative[Parser].pure(a)

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

  def charCaseInsensitive(c: Char): Parser[Char] = anyChar.filter(c1 => c1.toLower == c.toLower)

  def string(s: String): Parser[String] = s.toList.traverse(char).map(_.mkString)

  def stringCaseInsensitive(s: String): Parser[String] =
    s.toList.traverse(charCaseInsensitive).map(_.mkString)

  def boolean: Parser[Boolean] =
    string("true").as(true).orElse(string("false").as(false))

  def booleanCaseInsensitive: Parser[Boolean] =
    choose(stringCaseInsensitive("true").as(true), stringCaseInsensitive("False").as(false))

  val openBrace: Parser[Unit]  = char('(').void
  val closeBrace: Parser[Unit] = char(')').void
  val newline: Parser[Unit]    = char('\n').void
  val space: Parser[Unit]      = char(' ').void

  extension [A](p: Parser[A]) {

    def repeat: Parser[NonEmptyList[A]] =
      p.map(a => NonEmptyList.of(a)).flatMap { as =>
        p.repeat.map(b => as <+> b).orElse(pure(as))
      }

    def repeat0: Parser[List[A]] =
      p.repeat.map(_.toList).orElse(pure(List.empty))

  }

  def parenthesis[A](parser: Parser[A]): Parser[A] =
    openBrace.void *> parser <* closeBrace.void

  val spaces: Parser[Unit] = space.repeat0.void

  def digit: Parser[Char] = anyChar.filter(_.isDigit)

  def digits: Parser[Int] = digit.repeat.mapFilter(_.toList.mkString.toIntOption)
