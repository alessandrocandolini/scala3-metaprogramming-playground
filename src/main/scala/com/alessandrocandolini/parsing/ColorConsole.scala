package com.alessandrocandolini.parsing

object ColorConsole:

  // ANSI escape codes for text colors
  val ANSI_RESET = "\u001B[0m"
  val ANSI_GREEN = "\u001B[32m"
  val ANSI_RED   = "\u001B[31m"

  // ANSI escape codes for symbols
  val TICK  = "\u2713" // Green tick
  val CROSS = "\u2717" // Red cross

  // Function to print text in green
  def printGreen(text: => String): String =
    ANSI_GREEN + text + ANSI_RESET

  // Function to print text in red
  def printRed(text: => String): String =
    ANSI_RED + text + ANSI_RESET

  // Function to print green tick
  val greenTick: String =
    ANSI_GREEN + TICK + ANSI_RESET

  // Function to print red cross
  val redCross: String =
    ANSI_RED + CROSS + ANSI_RESET

  def printColourMessage(message : String, isSuccessful: Boolean) : String =
    if (isSuccessful) {
      greenTick ++ " " ++ printGreen(message)
    } else {
      redCross ++ " " ++ printRed(message)
    }
