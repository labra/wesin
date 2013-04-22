// needed for custom scalastyle package
// resolvers += "namin.github.com/maven-repository" at "http://namin.github.com/maven-repository/"

// resolvers += "Spray Repository" at "http://repo.spray.cc/"

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"


// libraryDependencies += "net.databinder" %% "dispatch-http" % "0.8.8"

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.2.0")


// libraryDependencies += "cc.spray" %%  "spray-json" % "1.1.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.10" % "1.6.1"

// dispatch uses commons-codec, in version 1.4, so we can't  go for 1.6.
// libraryDependencies += "commons-codec" % "commons-codec" % "1.4"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.2")

scalacOptions ++= Seq("-deprecation")


// for dependency-graph plugin
// net.virtualvoid.sbt.graph.Plugin.graphSettings


// [info] default:default-3fdafc_2.9.1:0.1-SNAPSHOT
// [info]   +-cc.spray:spray-json_2.9.1:1.1.1
// [info]   | +-org.parboiled:parboiled-scala:1.0.2
// [info]   | | +-org.parboiled:parboiled-core:1.0.2
// [info]   | | +-org.scala-lang:scala-library:2.9.1
// [info]   | | 
// [info]   | +-org.scala-lang:scala-library:2.9.1
// [info]   | 
// [info]   +-com.typesafe.sbteclipse:sbteclipse-plugin:2.1.0
// [info]   | +-com.typesafe.sbteclipse:sbteclipse-core:2.1.0
// [info]   |   +-org.scalaz:scalaz-core_2.9.1:6.0.3
// [info]   |     +-org.scala-lang:scala-library:2.9.1
// [info]   |     
// [info]   +-net.databinder:dispatch-http_2.9.1:0.8.8
// [info]   | +-net.databinder:dispatch-core_2.9.1:0.8.8
// [info]   | | +-org.apache.httpcomponents:httpclient:4.1.3
// [info]   | | | +-commons-codec:commons-codec:1.4
// [info]   | | | +-commons-logging:commons-logging:1.1.1
// [info]   | | | +-org.apache.httpcomponents:httpcore:4.1.4
// [info]   | | | 
// [info]   | | +-org.scala-lang:scala-library:2.9.1
// [info]   | | 
// [info]   | +-net.databinder:dispatch-futures_2.9.1:0.8.8
// [info]   | | +-org.scala-lang:scala-library:2.9.1
// [info]   | | 
// [info]   | +-org.apache.httpcomponents:httpclient:4.1.3
// [info]   | | +-commons-codec:commons-codec:1.4
// [info]   | | +-commons-logging:commons-logging:1.1.1
// [info]   | | +-org.apache.httpcomponents:httpcore:4.1.4
// [info]   | | 
// [info]   | +-org.scala-lang:scala-library:2.9.1
// [info]   | 
// [info]   +-org.scala-lang:scala-library:2.9.1
// [info]   +-org.scalastyle:scalastyle_2.9.1:0.1.3-SNAPSHOT
// [info]   | +-com.github.scopt:scopt_2.9.1:2.0.0
// [info]   | | +-org.scala-lang:scala-library:2.9.1
// [info]   | | 
// [info]   | +-org.scalariform:scalariform_2.9.1:0.1.1
// [info]   |   +-org.scala-lang:scala-library:2.9.1
// [info]   |   
// [info]   +-org.scalatest:scalatest_2.9.1:1.8
// [info]     +-org.scala-lang:scala-library:2.9.1
