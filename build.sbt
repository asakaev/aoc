val dottyVersion = "3.0.0-M2"

// -new-syntax -rewrite
// -indent -rewrite
Compile / scalacOptions ++= List(
  "-explain",
  "-explain-types",
  "-new-syntax",
  "-rewrite"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
