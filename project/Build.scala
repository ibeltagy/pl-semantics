import sbt._
import Keys._

object MlnSemanticsBuild extends Build {

  lazy val main = Project(id = "mln-semantics", base = file(".")) dependsOn(dependent)

  lazy val dependent = Project(id = "scala-logic", base = file("scala-logic"))

}

