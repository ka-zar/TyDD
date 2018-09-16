
name := "TyDD"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions ++= Seq(
  "-feature",
  "-Ypartial-unification",
  "-language:higherKinds",
  "-Xprint:typer"
)