import sbt._
import sbt.Keys._
import bintray.Plugin.bintraySettings
import bintray.Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._

lazy val root = project.in(file("."))//.settings(crossScalaVersions := Seq("2.10.4", "2.11.1"))

Build.sharedSettings

version := Build.currentVersion

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
    "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.5"
  , "com.typesafe" % "config" % "1.2.0"
  , "org.scala-lang" % "scala-compiler" % scalaVersion.value 
  , "com.assembla.scala-incubator" %% "graph-core" % "1.9.0"
  , "org.apache.jena" % "jena-arq" % "2.11.1"
  , "com.lihaoyi" %% "utest" % "0.1.3" % "test"
  , "junit" % "junit" % "4.10" % "test"
  , "org.openrdf.sesame" % "sesame-model" % "2.7.10"
  , "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
  , "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
  , "es.weso" % "stateparser_2.11" % "0.0.3"
  , "es.weso" % "tgraph_2.11" % "0.0.1"
  )

// testFrameworks += new TestFramework("utest.runner.JvmFramework")

addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2")

autoCompilerPlugins := true

bintraySettings

Build.publishSettings

/* The following line is to download test files from W3c.
   It would be better to do it only when there is internet connection 
   */
resourceGenerators in Test += Def.task {
  val location = url("http://www.w3.org/2013/TurtleTests/TESTS.zip")
  IO.unzipURL(location, resourceManaged.value / "downloadedTests").toSeq
}.taskValue

// seq(bintrayPublishSettings:_*)

// resolvers += bintray.Opts.resolver.repo("weso", "weso-releases")

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

