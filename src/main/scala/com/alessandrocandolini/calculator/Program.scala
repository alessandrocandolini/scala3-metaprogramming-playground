package com.alessandrocandolini.calculator

import cats.implicits.*
import cats.effect.{ExitCode, IO}
import com.alessandrocandolini.calculator.Interpreter.evaluate

enum ProgramError:
  case ParsingError
  case InterpreterError(e: EvalError)

object ProgramError:
  extension (e: ProgramError) {
    def consoleMessage: String = e match
      case ProgramError.ParsingError                                   => "parser error"
      case ProgramError.InterpreterError(EvalError.CannotDivideByZero) => s"division by zero is not defined"
  }

object Program:

  def run(s: String): IO[ExitCode] =
    logic(s) match {
      case Left(value)  => IO.println(s"Failure: ${value.consoleMessage}").as(ExitCode.Error)
      case Right(value) => IO.println(s"Result: $value").as(ExitCode.Success)
    }

  def logic(s: String): Either[ProgramError, Int] = for {
    ast <- GrammarParser.parseAst(s).toRight(ProgramError.ParsingError)
    res <- ast.evaluate.leftMap(ProgramError.InterpreterError.apply)
  } yield res
