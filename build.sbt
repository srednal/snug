organization := "com.srednal"
name := "snug"
version := "1.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0",
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % Test,
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.mockito" % "mockito-core" % "2.17.0" % Test
)

scalacOptions := Seq(
  "-Xlint",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Xfatal-warnings",
  "-Ywarn-value-discard",
  "-Ywarn-unused"
)

scalastyleFailOnWarning := true
coverageMinimum := 85
coverageFailOnMinimum := true
