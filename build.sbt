import com.typesafe.sbt.SbtStartScript

name := "mln-semantics"

version := "0.0.1"

scalaVersion := "2.10.6"

resolvers ++= Seq(
  DefaultMavenRepository,
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2",
  "repo.codahale.com" at "http://repo.codahale.com",
  "Clojars" at "http://www.clojars.org/repo",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "xalan" % "xalan" % "2.7.2",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0",
  "commons-logging" % "commons-logging" % "1.1.1",
	"com.googlecode.aima-java" % "aima-core" % "0.10.5",
   "com.assembla.scala-incubator" %% "graph-core" % "1.7.3",
  "log4j" % "log4j" % "1.2.16" excludeAll(
    ExclusionRule(organization = "javax.mail"),
    ExclusionRule(organization = "javax.jms"),
    ExclusionRule(organization = "com.sun.jdmk"),
    ExclusionRule(organization = "com.sun.jmx")
  ),
  "org.scalanlp" % "breeze-core_2.10" % "0.4",
  "org.scalanlp" % "breeze-math_2.10" % "0.4",
  "junit" % "junit" % "4.10" % "test")
  //"com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...


////////////////////////////////////////////////////////
// BEGIN FOR SCRUNCH
////////////////////////////////////////////////////////

resolvers ++= Seq(
  "Cloudera Hadoop Releases" at "https://repository.cloudera.com/content/repositories/releases/",
  "Thrift location" at "http://people.apache.org/~rawson/repo/"
)

libraryDependencies ++= Seq(
  "org.apache.hadoop" % "hadoop-core" % "0.20.2-cdh3u1" excludeAll(
    ExclusionRule(organization = "com.sun.jdmk"),
    ExclusionRule(organization = "com.sun.jmx"),
    ExclusionRule(organization = "javax.jms")
  ),
  "com.google.guava" % "guava" % "r09",
  "org.apache.avro" % "avro-mapred" % "1.6.0",
  "org.codehaus.jackson" % "jackson-core-asl" % "1.8.3",
  "org.codehaus.jackson" % "jackson-mapper-asl" % "1.8.3",
  "org.codehaus.jackson" % "jackson-smile" % "1.8.6",
  "org.slf4j" % "slf4j-log4j12" % "1.6.1",
  "org.apache.lucene" % "lucene-core" % "4.2.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.2.0",
  "org.apache.lucene" % "lucene-queryparser" % "4.2.0",
  "org.apache.hbase" % "hbase" % "0.90.3-cdh3u1" excludeAll(
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

seq(SbtStartScript.startScriptForClassesSettings: _*)

mainClass in (Compile, run) := Some("utcompling.mlnsemantics.run.Sts")

scalacOptions ++= Seq("-optimize", "-deprecation", "-unchecked")
