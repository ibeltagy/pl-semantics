import sbt._
import Keys._

object MlnSemanticsBuild extends Build {

  lazy val main = Project("mln-semantics", file(".")) dependsOn(scalaLogic) dependsOn(psl)

  lazy val scalaLogic = Project("scala-logic", file("scala-logic"))

  lazy val psl = Project("psl", file("psl"))

}

