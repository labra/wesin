import sbt._
import sbt.Keys._

lazy val root = project.in(file("."))

organization := "es.weso"

name := "wesin"

version := "0.4.9"

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
//  , "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  , "org.scalatest" %% "scalatest" % "2.2.6" % "test"
  , "es.weso" % "stateparser_2.11" % "0.1.2"
  , "es.weso" % "srdf-jvm_2.11" % "0.0.4"
  , "es.weso" % "tgraph_2.11" % "0.0.1"
  , "es.weso" % "turtleparser-jvm_2.11" % "0.0.5"
  )

// scalariformSettings

bintrayRepository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))

resolvers += Resolver.bintrayRepo("labra", "maven")

resolvers += Resolver.bintrayRepo("weso", "weso-releases")

