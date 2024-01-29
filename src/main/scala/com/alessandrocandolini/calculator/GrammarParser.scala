package com.alessandrocandolini.calculator

import cats.implicits.*
import com.alessandrocandolini.calculator.AstF.{Ast, *}
import com.alessandrocandolini.calculator.Parser.{char, choose, digits, double, parenthesis, parseAll, string}

import scala.language.implicitConversions

object GrammarParser:

  def predefined: Ast[Double] = 2 * (3 + 4)

  def preprocess(s: String): String = s.trim.replaceAll(" ", "")

  val fakeParser = string("2*(3+4)").as(predefined)

  val parser: Parser[Ast[Double]] = fakeParser.orElse(realParser)

  val literalP: Parser[Ast[Double]] = double
    .map(literal)

  val binaryOperationP: Parser[Ast[Double]] = parenthesis((realParser, operationParser, realParser).mapN {
    case (v1, op, v2) =>
      binaryOperation(op, v1, v2)
  })

  def realParser: Parser[Ast[Double]] =
    literalP.orElse(binaryOperationP)

  def parseAst(s: String): Option[Ast[Double]] =
    parser.parseAll(preprocess(s))

  def operationParser: Parser[BinaryOperation] = choose(
    char('+').as(BinaryOperation.Add),
    char('*').as(BinaryOperation.Multiply)
  )
