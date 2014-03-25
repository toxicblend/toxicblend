import sbt._
import Keys._

object Projects {
  lazy val jbulletd =  RootProject(uri("https://github.com/toxicblend/jbulletd.git"))  
  lazy val toxiclibs =  RootProject(uri("hg:https://bitbucket.org/ead_fritz/toxiclibs/"))
}