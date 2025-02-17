ThisBuild / version := "1.2.0"

ThisBuild / scalaVersion := "3.6.3"
val org = "com.pyromuffin"
isSnapshot := true

githubTokenSource := TokenSource.Or(
  TokenSource.Environment("GITHUB_TOKEN"), // Injected during a github workflow for publishing
  TokenSource.GitConfig("github.token") // local token set in ~/.gitconfig
)

credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  "_",
  sys.env("GITHUB_TOKEN")
)

lazy val root = (project in file("."))
  .settings(
    name := "Zobjectifier",
    organization := org,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    githubOwner := "pyromuffin",
    githubRepository := "zobjectifier",
  )

