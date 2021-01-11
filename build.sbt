organization := "com.srednal"
name := "snug"
version := "1.1"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.4.1",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % Test,
  "org.scalatest" %% "scalatest" % "3.2.3" % Test,
  "org.mockito" % "mockito-core" % "3.7.0" % Test
)

scalacOptions := Seq(
  "-Xlint:_",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Xfatal-warnings",
  "-Ywarn-value-discard",
  "-Ywarn-unused:_",
  "-explaintypes",
  "-Xcheckinit",
  "-Ywarn-dead-code",
  "-Ywarn-extra-implicit"
)

scalastyleFailOnWarning := true
coverageMinimum := 85
coverageFailOnMinimum := true

// don't fail on warnings in the console, otherwise you can't import anything (it's unused)
scalacOptions in (Compile, console) := Nil
scalacOptions in (Test, console) := Nil
