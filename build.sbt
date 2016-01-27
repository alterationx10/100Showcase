name := """100showcase"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  cache,
  ws,
  "com.typesafe.play" %% "play-slick" % "1.1.1",
  "com.h2database" % "h2" % "1.4.190",
  "org.postgresql" % "postgresql" % "9.4.1207",
  "org.webjars" % "bootstrap" % "3.3.6",
  "com.amazonaws" % "aws-java-sdk" % "1.10.49"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator
