lazy val root = project
  .in(file("."))
  .settings(
    name := "ulc",
    version := "0.1.0",
    scalaVersion := "3.3.1",
    scalacOptions ++= Seq(
      "-Wunused:all",
      "-Wnonunit-statement",
      "-Wvalue-discard",
      "-language:strictEquality"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-parse" % "0.3.9"
    )
  )
