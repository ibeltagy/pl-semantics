import AssemblyKeys._

import com.typesafe.startscript.StartScriptPlugin

name := "mln-semantics"

version := "0.0.1"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "edu.stanford.nlp" % "stanford-corenlp" % "1.3.0",
  "org.apache.hadoop" % "hadoop-core" % "0.20.2-cdh3u1" excludeAll(
    ExclusionRule(organization = "com.sun.jdmk"),
    ExclusionRule(organization = "com.sun.jmx"),
    ExclusionRule(organization = "javax.jms")
  ),
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16" excludeAll(
    ExclusionRule(organization = "javax.mail"),
    ExclusionRule(organization = "javax.jms"),
    ExclusionRule(organization = "com.sun.jdmk"),
    ExclusionRule(organization = "com.sun.jmx")
  ),
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...


////////////////////////////////////////////////////////
// BEGIN FOR SCRUNCH
////////////////////////////////////////////////////////

resolvers ++= Seq(
  "Cloudera Hadoop Releases" at "https://repository.cloudera.com/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "com.cloudera.crunch" % "crunch" % "0.2.0" excludeAll(
    ExclusionRule(organization = "com.sun.jdmk"),
    ExclusionRule(organization = "com.sun.jmx"),
    ExclusionRule(organization = "javax.jms")
  ),
  "org.apache.hbase" % "hbase" % "0.90.3-cdh3u1" % "provided" excludeAll(
    ExclusionRule(organization = "org.apache.hadoop"),
    ExclusionRule(organization = "commons-logging"),
    ExclusionRule(organization = "com.google.guava"),
    ExclusionRule(organization = "log4j"),
    ExclusionRule(organization = "org.slf4j")
  )
)

////////////////////////////////////////////////////////
// END FOR SCRUNCH
////////////////////////////////////////////////////////


seq(StartScriptPlugin.startScriptForClassesSettings: _*)

seq(assemblySettings: _*)
