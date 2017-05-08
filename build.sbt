name := "sample2"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.0",
  "com.typesafe.akka" % "akka-slf4j_2.11" % "2.5.1",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)
