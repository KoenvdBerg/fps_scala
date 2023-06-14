ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "academy"
  )


libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.8.1"

ThisBuild / scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:higherKinds",
//  "-explain"
)

