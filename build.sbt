name := "advent-of-code-2017"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions += "-Ypartial-unification"

testOptions in Test += Tests.Argument("-oD")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0-RC1",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)
