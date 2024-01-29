package com.alessandrocandolini.calculator

import cats.implicits.*
import com.alessandrocandolini.calculator.AstF.{Ast, *}
import com.alessandrocandolini.calculator.Parser.{chainl1, char, choose, double, parenthesis, parseAll}

object GrammarParser:

  def parseAst(s: String): Option[Ast[Double]] =
    parser.parseAll(preprocess(s))

  private def preprocess(s: String): String =
    s.trim.replaceAll(" ", "")

  private def parser: Parser[Ast[Double]] =
    chainl1(
      highPriorityParser,
      lowPriorityOperation.map(op => a1 => a2 => binaryOperation(op, a1, a2))
    )

  private def highPriorityParser: Parser[Ast[Double]] =
    chainl1(
      parenthesisedParser,
      highPriorityOperation.map(op => a1 => a2 => binaryOperation(op, a1, a2))
    )

  private def parenthesisedParser: Parser[Ast[Double]] =
    literalP.orElse(parenthesis(parser))

  private val literalP: Parser[Ast[Double]] = double.map(literal)

  def lowPriorityOperation: Parser[BinaryOperation] = choose(
    char('+').as(BinaryOperation.Add),
    char('-').as(BinaryOperation.Subtract)
  )

  def highPriorityOperation: Parser[BinaryOperation] = choose(
    char('*').as(BinaryOperation.Multiply),
    char('/').as(BinaryOperation.Divide)
  )
