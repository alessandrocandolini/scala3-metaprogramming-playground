package com.alessandrocandolini.macros

import java.time.ZonedDateTime
import com.alessandrocandolini.macros.ZonedDateTimeMacro.date

object ZonedDateTimeExample:

  val example1: ZonedDateTime = date"""
         2021-12-03T10:15:30Z
    """

  val example2: ZonedDateTime = date"""
         2021a-02-03T10:15:30Z
    """
