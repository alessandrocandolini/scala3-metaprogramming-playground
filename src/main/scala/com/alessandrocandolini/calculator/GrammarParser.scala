package com.alessandrocandolini.calculator

import cats.implicits.*
import com.alessandrocandolini.calculator.AstF.{Ast, *}
import com.alessandrocandolini.calculator.Parser.{chainl1, char, choose, double, parenthesis, parseAll}

object GrammarParser:

  def preprocess(s: String): String = s.trim.replaceAll(" ", "")

  def parser: Parser[Ast[Double]] =
    chainl1(termParser, lowPriorityOperation.map(op => a1 => a2 => binaryOperation(op, a1, a2)))

  def termParser: Parser[Ast[Double]] =
    chainl1(
      parenthesisedParser,
      highPriorityOperation.map(op => a1 => a2 => binaryOperation(op, a1, a2))
    )

  def parenthesisedParser: Parser[Ast[Double]] =
    literalP.orElse(parenthesis(parser))

  val literalP: Parser[Ast[Double]] = double.map(literal)

  def parseAst(s: String): Option[Ast[Double]] =
    parser.parseAll(preprocess(s))

  def lowPriorityOperation: Parser[BinaryOperation] = choose(
    char('+').as(BinaryOperation.Add),
    char('-').as(BinaryOperation.Subtract)
  )

  def highPriorityOperation: Parser[BinaryOperation] = choose(
    char('*').as(BinaryOperation.Multiply),
    char('/').as(BinaryOperation.Divide)
  )
