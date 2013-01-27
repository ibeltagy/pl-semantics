name := "scala-logic"

version := "0.0.1"

scalaVersion := "2.9.2"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2",
  "repo.codahale.com" at "http://repo.codahale.com",
  "Breeze Maven2" at "http://repo.scalanlp.org/repo",
  "Clojars" at "http://www.clojars.org/repo"
)

libraryDependencies ++= Seq(
  "log4j" % "log4j" % "1.2.16",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test->default") //switch to ScalaTest at some point...

