import sbt._
import sbt.Keys._
import bintray.Plugin.bintraySettings
import bintray.Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._

lazy val root = project.in(file("."))//.settings(crossScalaVersions := Seq("2.10.4", "2.11.0"))

Build.sharedSettings

version := Build.currentVersion

libraryDependencies ++= Seq(
    "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.1" 
  , "com.typesafe" % "config" % "1.2.0"
  , "org.scala-lang" % "scala-compiler" % "2.10.3" 
  , "com.assembla.scala-incubator" % "graph-core_2.10" % "1.7.3"
  , "org.apache.jena" % "jena-arq" % "2.11.1" 
  , "com.lihaoyi" %% "utest" % "0.1.3" % "test"
  , "org.scalatest" % "scalatest_2.10" % "2.1.0-RC2"
  , "junit" % "junit" % "4.10" % "test"
  , "org.openrdf.sesame" % "sesame-model" % "2.7.10"
  , "es.weso" % "stateparser_2.10" % "0.0.1" 
)

testFrameworks += new TestFramework("utest.runner.JvmFramework")

addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2")

autoCompilerPlugins := true

bintraySettings

Build.publishSettings

// resolvers += bintray.Opts.resolver.repo("weso", "weso-releases")

// bintray.Plugin.bintrayResolverSettings

