import sbt._
import sbt.Keys._
import bintray.Plugin.bintraySettings
import bintray.Keys._

/**
 * this files is intended to build the main project
 * it contains links to all dependencies that are needed
 * */
object WesinBuild extends Build
{


  /**
   * Wesin dependencies
   */
  val wesinDeps = Seq(
    "org.scalatest" % "scalatest_2.10" % "2.1.3",

    "com.assembla.scala-incubator" % "graph-core_2.10" % "1.7.3", //TODO: fix errors in tgraphimpl to migrate to 1.8.0

    "org.apache.commons" % "commons-lang3" % "3.1"
  )



  val wesinResolvers = Seq(
    "namin.github.com/maven-repository" at "http://namin.github.com/maven-repository/",
    "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
  )

  lazy val publishSettings = Seq(
    bintrayOrganization in bintray := Some("es.weso")
  )

  lazy val wesinSettings =
    Seq(
      name := "wesin",
      scalacOptions ++= Seq( "-feature", "-language:_" ),
      version := "0.1.0",
      scalaVersion := "2.10.3",
      resolvers ++= wesinResolvers,
      libraryDependencies ++=wesinDeps,
      organization := "es.weso",
      licenses += ("MPL", url("http://opensource.org/licenses/MPL-2.0"))
    ) ++ bintraySettings ++ publishSettings




  lazy val wesin  = Project(
    id   = "wesin",
    base = file(".")
  ) settings (wesinSettings: _*)



}// needed for custom scalastyle package
