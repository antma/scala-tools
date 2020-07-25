name := "scala-tools"

organization := "org.lichess"

version := "1.0"

scalaVersion := "2.13.3"

libraryDependencies ++= List(
  "org.specs2"             %% "specs2-core"              % "4.7.0" % "test"
)

scalacOptions ++= Seq(
  "-language:implicitConversions",
  "-language:postfixOps",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xlint:_",
  "-Ywarn-macros:after",
  "-Ywarn-unused:_",
  "-Xmaxerrs",
  "12",
  "-Xmaxwarns",
  "12"
)

