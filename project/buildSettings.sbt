// needed for custom scalastyle package
resolvers += "namin.github.com/maven-repository" at "http://namin.github.com/maven-repository/"

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.2.0")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.10" % "1.6.1"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.2")

scalacOptions ++= Seq("-deprecation")