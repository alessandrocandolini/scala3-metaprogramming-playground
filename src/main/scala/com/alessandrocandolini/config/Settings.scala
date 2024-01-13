package com.alessandrocandolini.config

import cats.effect.IO
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.module.catseffect.syntax.*
import com.alessandrocandolini.utils.SnakecasePureconfigDerive.derived

case class Settings(
  port: Int,
  host: String,
) derives CanEqual, ConfigReader

object Settings:

  def readConfigOrThrow: IO[Settings] = ConfigSource.default.loadF[IO, Settings]()
