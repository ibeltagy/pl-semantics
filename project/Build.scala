import sbt._
import Keys._

object MlnSemanticsBuild extends Build {

  lazy val main = Project("mln-semantics", file(".")) aggregate(scalaLogic) dependsOn(scalaLogic)

  lazy val scalaLogic = Project("scala-logic", file("scala-logic")) aggregate(scalabha) dependsOn(scalabha)

  lazy val scalabha = Project("Scalabha", file("scala-logic/scalabha"))

}

