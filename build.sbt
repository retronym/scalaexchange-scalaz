name := "scalaexchange-scalaz"

version := "1.0"

resolvers += ScalaToolsSnapshots

scalaVersion := "2.9.0-1"

libraryDependencies += "org.scalaz" % "scalaz-core_2.9.0-1" % "6.0.1" withSources()

libraryDependencies += "org.scalaz" % "scalaz-example_2.9.0-1" % "6.0.1" intransitive() withSources()