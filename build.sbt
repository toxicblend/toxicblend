lazy val toxicblend = project.in( file(".")).aggregate(Projects.jbulletd, Projects.toxiclibs).dependsOn( Projects.toxiclibs, Projects.jbulletd )

version := "0.1"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
	"org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
	"java3d" % "vecmath" % "1.5.2"
)

resolvers += "Geotoolkit.org" at "http://maven.geotoolkit.org"

publishMavenStyle := true

publishArtifact in Test := false

publishTo := Some(Resolver.file("file",  baseDirectory.value / "dist" ) )

net.virtualvoid.sbt.graph.Plugin.graphSettings
