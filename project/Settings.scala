import sbt._
import Keys._

object Settings {

  val commonSettings: Seq[Def.Setting[_]] = Seq(
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
//      "-language:strictEquality",
      "-language:postfixOps",
//      "-Yexplicit-nulls",
      "-source:future",
      "-explain",
      "-Wvalue-discard",
      "-Wunused:all",
      "-Xcheck-macros",
      "-Ykind-projector:underscores"
    )
  )

}

object Dependencies {

  val dependencies : Seq[ModuleID] = {

    val cats = Seq(
      "org.typelevel" %% "cats-core" % Versions.cats,
      "org.typelevel" %% "cats-effect" % Versions.catsEffect,
      "org.typelevel" %% "kittens" % Versions.kittens
    )

    val fs2 = Seq(
      "fs2-core",
      "fs2-io"
    ).map("co.fs2" %% _ % Versions.fs2)

    val pureConfig = Seq(
      "pureconfig-core", "pureconfig-cats-effect"
    ).map("com.github.pureconfig" %% _ % Versions.pureConfig)

    val tapir = Seq(
      "tapir-core",
      "tapir-sttp-client",
      "tapir-json-circe").map(
      "com.softwaremill.sttp.tapir" %% _ % Versions.tapir
    )

    val decline = Seq(
      "decline-effect",
      "decline"
    ).map("com.monovore" %% _ % Versions.decline)

    val advanced = Seq(
      "io.github.arainko" %% "ducktape" % Versions.ducktape,
      "io.github.iltotore" %% "iron" % Versions.iron,
      "org.typelevel" %% "cats-parse" % Versions.catsParse,
      "io.higherkindness" %% "droste-core" % Versions.droste
    )

    val postgres = Seq(
      "org.postgresql" % "postgresql" % Versions.postgres,
      "org.tpolecat" %% "skunk-core" % Versions.skunk
    )
    val circe = Seq("circe-core").map("io.circe" %% _ % Versions.circe) ++
       Seq("io.circe" %% "circe-fs2" % "0.14.0")

    val sttp = Seq("core", "circe", "async-http-client-backend-cats",  "slf4j-backend").map(
      "com.softwaremill.sttp.client3" %% _  % Versions.sttp
    )

    val nettyOverrides = Seq("netty-handler", "netty-codec-http2").map("io.netty" % _  % Versions.netty)

    cats ++ fs2 ++ pureConfig ++ tapir ++ circe ++ sttp ++ decline ++ advanced ++ postgres ++ nettyOverrides

  }

  val testDependencies: Seq[ModuleID] = {
    val munit = Seq(
      "munit-scalacheck",
      "munit"
    ).map("org.scalameta" %% _ % Versions.munit)

    val scalacheck = Seq(
      "org.scalacheck" %% "scalacheck"                  % Versions.scalacheck,
      "org.typelevel"  %% "scalacheck-effect"           % Versions.scalacheckEffect,
      "com.47deg"      %% "scalacheck-toolbox-datetime" % Versions.scalaCheckToolbox
    )

    val extras = Seq(
      "org.typelevel"   %% "munit-cats-effect"            % Versions.munitCE,
      "org.typelevel"   %% "scalacheck-effect-munit"        % Versions.scalacheckEffect,
    )

    scalacheck ++ munit ++ extras
  }.map(_ % Test)

}

object Versions {

  val scala              = "3.6.3"
  val cats               = "2.13.0"
  val fs2                = "3.9.3"
  val catsEffect         = "3.6.0-RC1"
  val kittens            = "3.4.0"
  val pureConfig         = "0.17.5"
  val decline            = "2.4.1"
  val catsParse          = "1.0.0"
  val ducktape           = "0.2.0"
  val iron               = "2.6.0"
  val droste             = "0.9.0"

  val tapir              = "1.10.4"
  val circe              = "0.14.9"
  val sttp               = "3.9.1"
  val netty              = "4.1.107.Final"

  val postgres           = "42.7.0"
  val skunk              = "1.0.0-M8"

  val scalacheck         = "1.17.0"
  val scalacheckEffect   = "1.0.4"
  val scalaCheckToolbox  = "0.7.0"
  val munit              = "1.1.0"
  val munitCE            = "2.0.0-M4"

}
