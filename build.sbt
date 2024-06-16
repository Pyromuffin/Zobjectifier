ThisBuild / version := "1.0.4"

ThisBuild / scalaVersion := "3.2.2"
val org = "com.pyromuffin"
isSnapshot := true

githubTokenSource := TokenSource.Or(
  TokenSource.Environment("GITHUB_TOKEN"), // Injected during a github workflow for publishing
  TokenSource.GitConfig("github.token") // local token set in ~/.gitconfig
)

lazy val root = (project in file("."))
  .settings(
    name := "Zobjectifier",
    organization := org,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    githubOwner := "pyromuffin",
    githubRepository := "zobjectifier",
  )

