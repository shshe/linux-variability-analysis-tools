name := "lvat"

version := "0.5-SNAPSHOT"

organization := "ca.uwaterloo.gsd"

scalaVersion := "2.9.0-1"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-swing" % "2.9.0-1",
    "com.novocode" % "junit-interface" % "0.6" % "test",
    "junit" % "junit" % "4.8.2" % "test",
    "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
    "com.google.protobuf" % "protobuf-java" % "2.4.1",
    "com.googlecode" % "kiama_2.9.0-1" % "1.1.0"
)

resolvers += "Local Maven Repository" at Path.userHome.asURL + "/.m2/repository"

// only show 10 lines of stack traces
traceLevel := 10

javaOptions += "-Xss8192k -Xmx2048m"

scalacOptions := Seq("-deprecation", "-unchecked")

// workaround for "Scaladoc generation failed" on BuilderParent for Protobuf
publishArtifact in (Compile, packageDoc) := false 

