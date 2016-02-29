import sbt._
import sbt.Keys._

lazy val root = project.in(file("."))

organization := "es.weso"

name := "wesin"

version := "0.4.8"

scalaVersion := "2.11.7"

publishMavenStyle := true

libraryDependencies ++= Seq(
    "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.5"
  , "com.typesafe" % "config" % "1.2.0"
  , "org.scala-lang" % "scala-compiler" % scalaVersion.value 
  , "com.assembla.scala-incubator" %% "graph-core" % "1.9.0"
  , "org.apache.jena" % "jena-arq" % "2.13.0"
  , "junit" % "junit" % "4.10" % "test"
  , "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
  , "org.scalatest" %% "scalatest" % "2.2.6" % "test"
  , "es.weso" % "stateparser_2.11" % "0.1.0"
  , "es.weso" % "srdf-jvm_2.11" % "0.0.4"
  , "es.weso" % "tgraph_2.11" % "0.0.1"
//  , "org.w3" % "banana-rdf_2.11" % "0.8.1"
  )


addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2")

autoCompilerPlugins := true

// scalariformSettings

bintrayRepository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))


/* The following line is to download test files from W3c.
   It would be better to do it only when there is internet connection 
   */
// resourceGenerators in Test <+= Def.task {
//   val location = url("http://www.w3.org/2013/TurtleTests/TESTS.zip")
//  IO.unzipURL(location, resourceManaged.value / "downloadedTests").toSeq
// }

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

