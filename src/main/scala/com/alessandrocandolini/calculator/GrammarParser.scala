package com.alessandrocandolini.calculator

import cats.implicits.*
import com.alessandrocandolini.calculator.AstF.Ast
import com.alessandrocandolini.calculator.Parser.{char, choose, parseAll, string}

import scala.language.implicitConversions

object GrammarParser:

  def predefined: Ast[Int] = 2 * (3 + 4)

  def preprocess(s: String): String = s.trim.replaceAll(" ", "")

  def parser: Parser[Ast[Int]] =
    string("2*(3+4)").as(predefined)

  def parseAll(s: String): Option[Ast[Int]] =
    parser.parseAll(preprocess(s))

  def operationParser: Parser[BinaryOperation] = choose(
    char('+').as(BinaryOperation.Add),
    char('*').as(BinaryOperation.Multiply)
  )
