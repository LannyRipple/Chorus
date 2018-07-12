import com.spotright.sbt._

lazy val root =
    (project in file(".")).
    settings(spotright.settings: _*).
    settings(
        name := "chorus",
        organization := "com.spotright",
        version := "1.0.0",
        scalaVersion := "2.11.6",
        libraryDependencies ++= Seq(
            "com.spotright.common" %% "common-core" % "4.14.0"
        )
    )
