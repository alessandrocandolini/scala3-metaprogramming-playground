package com.alessandrocandolini.calculator

import cats.Applicative
import cats.data.{NonEmptyList, StateT}
import cats.implicits.*
import com.alessandrocandolini.parsing.ColorConsole.printColourMessage

type Parser[A] = StateT[Option, String, A]

object Parser:

  extension [A](p: Parser[A]) {
    def parse(s: String): Option[(String, A)] =
      p.run(s)

    def parseAll(s: String): Option[A] =
      parse(s).filter(_._1.isEmpty).map(_._2)

    // performs backtracking, use wisely and be careful about exponential worst-case complexity
    def orElse(p2: Parser[A]): Parser[A] =
      p.combineK(withLogs("orElse")(p2))
  }

  private def pure[A](a: A): Parser[A] = Applicative[Parser].pure(a)

  def choose[A](one: Parser[A], other: Parser[A]*): Parser[A] =
    NonEmptyList.fromList(other.toList) match {
      case Some(ps) => ps.foldLeft(one) { case (p1, p2) => p1.orElse(p2) }
      case None     => one
    }

  private def anyChar: Parser[Char] = StateT { s =>
    s.headOption.map { c =>
      s.tail -> c
    }
  }

  def char(c: Char): Parser[Char] =
    withLogs(s"char matching '$c'") {
      anyChar.filter { c1 =>
        c1 == c
      }
    }

  def charCaseInsensitive(c: Char): Parser[Char] =
    anyChar.filter(c1 => c1.toLower == c.toLower)

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
      p.flatMap { a =>
        p.repeat.map(as => NonEmptyList.of(a) <+> as).orElse(pure(NonEmptyList.of(a)))
      }

    def repeat0: Parser[List[A]] =
      p.repeat.map(_.toList).orElse(pure(List.empty))

  }

  def parenthesis[A](parser: Parser[A]): Parser[A] =
    openBrace *> parser <* closeBrace

  val spaces: Parser[Unit] = space.repeat0.void

  def digit: Parser[Char] =
    withLogs("char is digit") {
      anyChar.filter(_.isDigit)
    }

  def nonDigit: Parser[Char] =
    anyChar.filter(c => !c.isDigit)

  def digits: Parser[Int] = digit.repeat.mapFilter(_.toList.mkString.toIntOption)

  def alpha: Parser[String] = nonDigit.repeat.map(_.toList.mkString)

  def double: Parser[Double] = digit.repeat.flatMap { digits =>
    (char('.'), digit.repeat).mapN { case (dot, decimal) =>
      digits <+> NonEmptyList.of(dot) <+> decimal
    }.orElse(pure(digits))
  }.mapFilter(_.mkString_("").toDoubleOption)

  // left-associative parser for a infix binary operation
  def chainl1[A](p1: Parser[A], p2: Parser[A => A => A]): Parser[A] =
    p1.flatMap { a1 =>
      (p2, p1).mapN { case (operation, a2) => operation(a1)(a2) }.orElse(pure(a1))
    }

  private def withLogs[A](message: => String)(p: Parser[A]): Parser[A] = StateT { s =>
    val res = p.run(s)
    val m   = res match
      case Some((s1, a)) =>
        printColourMessage(s"$message [input = $s, output = $a, unparseInput = $s1 ]", true)
      case None          =>
        printColourMessage(s"$message [input = $s, output = None", false)
    println(m)
    res
  }
