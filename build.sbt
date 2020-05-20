name := "scala-tools"

organization := "org.lichess"

version := "1.0"

scalaVersion := "2.13.2"

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

