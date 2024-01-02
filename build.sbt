lazy val root = project
  .in(file("."))
  .settings(
    name := "ulc",
    version := "0.1.0",
    scalaVersion := "3.3.1",
    scalacOptions ++= Seq(
      "-Wunused:all",
      "-Wnonunit-statement",
      "-Wvalue-discard"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.lihaoyi" %% "fastparse" % "3.0.2"
    )
  )
