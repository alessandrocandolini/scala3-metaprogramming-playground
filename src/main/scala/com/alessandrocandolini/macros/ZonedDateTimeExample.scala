package com.alessandrocandolini.macros

import java.time.ZonedDateTime
import com.alessandrocandolini.macros.ZonedDateTimeMacro.iso8601

object ZonedDateTimeExample:

  val example1: ZonedDateTime = iso8601"""
         2021-12-03T10:15:30Z
    """

  // this would not compile
//  val example2: ZonedDateTime = date"""
//         2021-02-0T10:15:30Z
//    """
