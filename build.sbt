name := "StatsPDFMarking"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
	"org.apache.pdfbox" % "pdfbox" % "1.8.10",
	"com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
	"org.scalikejdbc" %% "scalikejdbc" % "2.2.7",
	//"ch.qos.logback" % "logback-classic" % "1.1.3",
	"com.github.tototoshi" %% "scala-csv" % "1.2.2",
	"mysql" % "mysql-connector-java" % "5.1.36",
	"pdeboer" %% "pplib" % "0.1-SNAPSHOT"
)

assemblyJarName in assembly := "papersampler.jar"

test in assembly := {}

mainClass in assembly := Some("paperSampler.PaperSampler")

assemblyMergeStrategy in assembly := {
	case "log4j.properties" => MergeStrategy.concat
	case "application.conf" => MergeStrategy.concat
	case "application.conf_default" => MergeStrategy.discard
	case "StaticLoggerBinder.class" => MergeStrategy.first
	case "StaticMDCBinder.class" => MergeStrategy.first
	case "StaticMarkerBinder.class" => MergeStrategy.first
	case x =>
		val oldStrategy = (assemblyMergeStrategy in assembly).value
		oldStrategy(x)
}