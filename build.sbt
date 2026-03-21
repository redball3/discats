
val toolkitV = "0.1.28"
val toolkit  = "org.typelevel"    %% "toolkit"      % toolkitV
val weaverV  = "0.8.4"

ThisBuild / scalaVersion  := "3.3.4"
ThisBuild / organization  := "io.github.discats"
ThisBuild / name          := "discats"
ThisBuild / version       := "0.0.1"

// Publishing
ThisBuild / homepage := Some(url("https://github.com/redball3/discats"))
ThisBuild / licenses := List("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / scmInfo := Some(ScmInfo(
  url("https://github.com/redball3/discats"),
  "scm:git:git@github.com:redball3/discats.git",
))
ThisBuild / developers := List(
  Developer("redball3", "Richie Lee", "", url("https://github.com/redball3")),
)
// Publishing — GitHub Packages
ThisBuild / publishTo := {
  val repo = sys.env.getOrElse("GITHUB_REPOSITORY", "redball3/discats")
  Some("GitHub Packages" at s"https://maven.pkg.github.com/$repo")
}
ThisBuild / credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  sys.env.getOrElse("GITHUB_ACTOR", ""),
  sys.env.getOrElse("GITHUB_TOKEN", ""),
)

libraryDependencies ++= Seq(
  toolkit,
  "io.circe"            %% "circe-generic" % "0.14.8",
  "com.disneystreaming" %% "weaver-cats"   % weaverV % Test,
)

testFrameworks += new TestFramework("weaver.framework.CatsEffect")
