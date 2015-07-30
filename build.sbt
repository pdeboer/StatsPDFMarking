name := "StatsPDFMarking"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.apache.pdfbox"           % "pdfbox"                % "1.8.7",
  "com.typesafe.scala-logging" %% "scala-logging"         % "3.1.0",
  "org.scalikejdbc"            %% "scalikejdbc"           % "2.2.7",
  "ch.qos.logback"              % "logback-classic"       % "1.1.3",
  "mysql"                       % "mysql-connector-java"  % "5.1.34"
)
