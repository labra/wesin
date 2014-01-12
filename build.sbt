name := "Wesin" 

version := "0.1.0"

scalaVersion := "2.10.2"

net.virtualvoid.sbt.graph.Plugin.graphSettings

scalacOptions ++= Seq("-deprecation")

libraryDependencies ++= Seq(
  "commons-configuration" % "commons-configuration" % "1.7",
  "org.rogach" %% "scallop" % "0.9.1" ,
  "com.typesafe" % "config" % "1.0.1",
  "org.scala-lang" % "scala-compiler" % "2.10.2" ,
  "com.assembla.scala-incubator" % "graph-core_2.10" % "1.6.2",
  "org.apache.jena" % "jena-arq" % "2.10.1" ,
  "org.scalatest" % "scalatest_2.10" % "2.0.1-SNAP" % "test",
  "junit" % "junit" % "4.10" % "test")

