import sbt._
import sbt.Keys._
import bintray.Plugin.bintraySettings
import bintray.Keys._
import Def.ScopedKey
import scala.scalajs.sbtplugin.ScalaJSPlugin._

/**
 * this files is intended to build the main project
 * it contains links to all dependencies that are needed
 * */
object Build extends sbt.Build {

  val isRelease = true 
 
  def repo = "weso-releases" 

  val wesinVersion = "0.1.5"
    
  publishMavenStyle := true

  val currentVersion = wesinVersion

  protected val bintrayPublishIvyStyle = settingKey[Boolean]("=== !publishMavenStyle") //workaround for sbt-bintray bug
  
  lazy val publishSettings = Seq(
    repository in bintray := this.repo,
    bintrayOrganization in bintray := Some("weso"),
    licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0")),
    bintrayPublishIvyStyle := false
  )


  /**
   * For parts of the project that we will not publish
   */
  lazy val noPublishSettings = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

  val sharedSettings = Seq(
      organization := "es.weso",
      name := "wesin",
      scalaVersion := "2.10.4"
    )

  val scalajsResolver: URLRepository = Resolver.url("scala-js-releases",
    url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(
      Resolver.ivyStylePatterns)

  
} // needed for custom scalastyle package
