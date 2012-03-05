import com.typesafe.startscript.StartScriptPlugin

name := "mln-semantics"

version := "0.0.1"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "edu.stanford.nlp" % "stanford-corenlp" % "1.3.0",
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...


//////////////////////////////////////////
// Begin Scoobi dependencies and options
//////////////////////////////////////////

libraryDependencies ++= Seq(
  "com.odiago.avro" % "odiago-avro" % "1.0.5",
  "javassist" % "javassist" % "3.12.1.GA",
  "org.apache.hadoop" % "hadoop-core" % "0.20.2-cdh3u1",
  "org.apache.avro" % "avro-mapred" % "1.6.0",
  "com.thoughtworks.xstream" % "xstream" % "1.4.2"
)

publishArtifact in packageDoc := false

resolvers ++= Seq(
  "Cloudera Maven Repository" at "https://repository.cloudera.com/content/repositories/releases/",
  "Packaged Avro" at "http://nicta.github.com/scoobi/releases/")

scalacOptions ++= Seq("-deprecation", "-Ydependent-method-types")

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")

//////////////////////////////////////////
// End Scoobi dependencies and options
//////////////////////////////////////////


seq(StartScriptPlugin.startScriptForClassesSettings: _*)
