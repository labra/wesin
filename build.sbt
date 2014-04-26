name := "Wesin" 

version := "0.1.0"

scalaVersion := "2.10.3"

net.virtualvoid.sbt.graph.Plugin.graphSettings

scalacOptions ++= Seq("-deprecation")

libraryDependencies ++= Seq(
    "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.1" 
  , "com.typesafe" % "config" % "1.2.0"
  , "org.scala-lang" % "scala-compiler" % "2.10.3" 
  , "com.assembla.scala-incubator" % "graph-core_2.10" % "1.7.3"
  , "org.apache.jena" % "jena-arq" % "2.11.1" 
  , "org.scalatest" % "scalatest_2.10" % "2.1.0-RC2"
  , "junit" % "junit" % "4.10" % "test"
  , "org.openrdf.sesame" % "sesame-model" % "2.7.10"
  , "es.weso" % "stateparser_2.10" % "0.0.1" 
)

resolvers += bintray.Opts.resolver.repo("weso", "weso-releases")

bintray.Plugin.bintrayResolverSettings

