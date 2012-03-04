import AssemblyKeys._

import com.typesafe.startscript.StartScriptPlugin

name := "mln-semantics"

version := "0.0.1"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "edu.stanford.nlp" % "stanford-corenlp" % "1.3.0",
  "org.apache.hadoop" % "hadoop-core" % "0.20.2-cdh3u1",
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...

resolvers += "Cloudera Maven Repository" at "https://repository.cloudera.com/content/repositories/releases/"

seq(assemblySettings: _*)

test in assembly := {}

seq(StartScriptPlugin.startScriptForClassesSettings: _*)
