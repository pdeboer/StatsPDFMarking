name := "StatsPDFMarking"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
	"org.apache.pdfbox" % "pdfbox" % "1.8.10",
	"com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
	"org.scalikejdbc" %% "scalikejdbc" % "2.2.7",
	"ch.qos.logback" % "logback-classic" % "1.1.3",
	"mysql" % "mysql-connector-java" % "5.1.36",
	"pdeboer" %% "pplib" % "0.1-SNAPSHOT"
)
