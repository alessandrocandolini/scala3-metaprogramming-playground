package com.alessandrocandolini.calculator

import cats.implicits.*
import com.alessandrocandolini.calculator.AstF.{Ast, *}
import com.alessandrocandolini.calculator.Parser.{char, choose, digits, double, parenthesis, parseAll, string}

import scala.language.implicitConversions

object GrammarParser:

  def predefined: Ast[Double] = 2 * (3 + 4)

  def preprocess(s: String): String = s.trim.replaceAll(" ", "")

  val fakeParser = string("2*(3+4)").as(predefined)

  val parser: Parser[Ast[Double]] = fakeParser.orElse(parenthesisedParser)

  val literalP: Parser[Ast[Double]] = double
    .map(literal)

  val parenthesisedBinaryOperationP: Parser[Ast[Double]] = parenthesis(
    (parenthesisedParser, operationParser, parenthesisedParser).mapN { case (v1, op, v2) =>
      binaryOperation(op, v1, v2)
    }
  )

  def parenthesisedParser: Parser[Ast[Double]] =
    literalP.orElse(parenthesisedBinaryOperationP)

  def parseAst(s: String): Option[Ast[Double]] =
    parser.parseAll(preprocess(s))

  def operationParser: Parser[BinaryOperation] = choose(
    char('+').as(BinaryOperation.Add),
    char('*').as(BinaryOperation.Multiply),
    char('-').as(BinaryOperation.Subtract),
    char('/').as(BinaryOperation.Divide)
  )
