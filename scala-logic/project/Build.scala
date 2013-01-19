import sbt._
import Keys._

object ScalaLogicBuild extends Build {

  lazy val main = Project("scala-logic", file(".")) dependsOn(scalabha)

  lazy val scalabha = Project("Scalabha", file("scalabha"))

}

