organization := "vision.id"
name := "tessella"
version := "0.2.1"

scalaVersion := "2.12.8"

resolvers ++= Seq(
  "NetBeans" at "http://bits.netbeans.org/nexus/content/groups/netbeans/",
  "gephi-thirdparty" at "https://raw.github.com/gephi/gephi/mvn-thirdparty-repo/"
)

libraryDependencies ++= Seq(
  "org.scala-graph" %% "graph-core" % "1.12.5",
  "org.scala-graph" %% "graph-constrained" % "1.12.5",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.1",
  "com.lihaoyi" %% "os-lib" % "0.2.6",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
  "org.gephi" % "gephi-toolkit" % "0.9.2" % Test classifier "all",
  "vision.id" %% "graphgephi" % "0.1.1" % Test,
  "com.storm-enroute" %% "scalameter" % "0.8.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false

Compile / unmanagedSourceDirectories += baseDirectory.value / "jvm" / "src"
Test / unmanagedSourceDirectories += baseDirectory.value / "jvm" / "test" / "src"