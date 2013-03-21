name := "lvat"

version := "1.0-SNAPSHOT"

organization := "com.googlecode.linux-variability-analysis-tools"

scalaVersion := "2.9.0-1"

licenses := Seq("GPL-2.0" -> url("http://www.opensource.org/licenses/gpl-2.0.php"))

homepage := Some(url("http://linux-variability-analysis-tools.googlecode.com"))

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

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
  <scm>
    <url>https://code.google.com/p/linux-variability-analysis-tools/</url>
    <connection>scm:hg:https://code.google.com/p/linux-variability-analysis-tools/</connection>
  </scm>
  <developers>
    <developer>
      <id>shshe</id>
      <name>Steven She</name>
      <url>http://www.stevenshe.ca</url>
    </developer>
  </developers>)
