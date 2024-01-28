package com.alessandrocandolini.calculator

import cats.implicits.*
import com.alessandrocandolini.calculator.AstF.{Ast, *}
import com.alessandrocandolini.calculator.Parser.{char, choose, digits, parenthesis, parseAll, string}

import scala.language.implicitConversions

object GrammarParser:

  def predefined: Ast[Int] = 2 * (3 + 4)

  def preprocess(s: String): String = s.trim.replaceAll(" ", "")

  val fakeParser = string("2*(3+4)").as(predefined)

  val parser: Parser[Ast[Int]] = fakeParser.orElse(realParser)

  val literalP: Parser[Ast[Int]] = digits
    .map(literal)

  val binaryOperationP: Parser[Ast[Int]] = (realParser, operationParser, realParser).mapN {
    case (v1, op, v2) =>
      binaryOperation(op, v1, v2)
  }

  def realParser: Parser[Ast[Int]] =
    parenthesis(binaryOperationP)
      .orElse(literalP)

  def parseAst(s: String): Option[Ast[Int]] =
    parser.parseAll(preprocess(s))

  def operationParser: Parser[BinaryOperation] = choose(
    char('+').as(BinaryOperation.Add),
    char('*').as(BinaryOperation.Multiply)
  )
