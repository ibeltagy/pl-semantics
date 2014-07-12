name := "PSL"

version := "0.0.1"

resolvers ++= Seq(
  "PSL Third Party" at "https://scm.umiacs.umd.edu/maven/lccd/content/repositories/psl-thirdparty/"
)


libraryDependencies ++= Seq(
	"edu.emory.mathcs" % "parallelcolt" % "0.9.4",
	"edu.emory.mathcs" % "csparsej" % "1.0",
	"edu.emory.mathcs" % "jplasma" % "1.2",
	"org.ujmp" % "ujmp-complete" % "0.2.4",
	"com.google.guava" % "guava" % "r09",
	"commons-configuration" % "commons-configuration" % "1.6",
	"commons-lang" % "commons-lang" % "2.6",
	"mysql" % "mysql-connector-java" % "5.0.5",
	"com.h2database" % "h2" % "1.2.126",
	"com.healthmarketscience.sqlbuilder" % "sqlbuilder" % "2.0.6",
	"de.mathnbits" % "mathnbitsSTL" % "1.0",
	"net.sourceforge.collections" % "collections-generic" % "4.01",
	"com.wcohen" % "secondstring" % "20120620",
	"log4j" % "log4j" % "1.2.16",
	"org.slf4j" % "slf4j-api" % "1.5.8",
	"org.slf4j" % "slf4j-log4j12" % "1.5.8"
)


