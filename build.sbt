import pl.project13.scala.sbt.JmhPlugin

lazy val root = Project("root", file("."))
  .settings(commonSettings)
  .settings(
    name := "Scala Golf",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test,
    ),
  )
  .enablePlugins(JmhPlugin)

lazy val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := "2.13.6",
  scalacOptions --= {
    if (sys.env.get("CI").isDefined) {
      Seq.empty
    } else {
      Seq("-Xfatal-warnings")
    }
  },
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
)
