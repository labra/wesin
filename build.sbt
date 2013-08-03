name := "Wesin" 

version := "1.0.0"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-deprecation")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
// libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.10" % "1.6.1"

// addSbtPlugin("com.eed3si9n" % "sbt-man" % "0.1.0")

// resolvers += Resolver.url("sbt-plugin-releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

