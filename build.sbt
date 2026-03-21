
val toolkitV = "0.1.28"
val toolkit  = "org.typelevel"    %% "toolkit"      % toolkitV
val weaverV  = "0.8.4"

ThisBuild / scalaVersion  := "3.3.4"
ThisBuild / organization  := "io.github.discats"
ThisBuild / name          := "discats"

// Publishing
ThisBuild / homepage := Some(url("https://github.com/discats/discats"))
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / scmInfo := Some(ScmInfo(
  url("https://github.com/discats/discats"),
  "scm:git:git@github.com:discats/discats.git",
))
ThisBuild / developers := List(
  Developer("discats", "discats", "", url("https://github.com/discats")),
)
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"
ThisBuild / publishTo              := sonatypePublishToBundle.value

libraryDependencies ++= Seq(
  toolkit,
  "io.circe"            %% "circe-generic" % "0.14.8",
  "com.disneystreaming" %% "weaver-cats"   % weaverV % Test,
)

testFrameworks += new TestFramework("weaver.framework.CatsEffect")
