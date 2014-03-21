name := "toxicblend"

version := "0.1"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
	"org.scalatest" % "scalatest_2.10" % "2.0" % "test",
	"java3d" % "vecmath" % "1.5.2"
)

resolvers += "Geotoolkit.org" at "http://maven.geotoolkit.org"

lazy val root = project.in( file(".")).aggregate(Projects.jbulletd).dependsOn(Projects.jbulletd)

