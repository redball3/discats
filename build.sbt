
val toolkitV    = "0.1.28"
val toolkit     = "org.typelevel" %% "toolkit"      % toolkitV
val toolkitTest = "org.typelevel" %% "toolkit-test" % toolkitV

ThisBuild / scalaVersion  := "3.3.4"
ThisBuild / organization  := "io.github.discats"
ThisBuild / name          := "discats"

libraryDependencies ++= Seq(
  toolkit,
  "io.circe" %% "circe-generic" % "0.14.8",
  toolkitTest % Test,
)
