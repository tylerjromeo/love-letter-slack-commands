import com.typesafe.sbt.packager.archetypes.JavaAppPackaging

name := "lidl-json-store"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "spray repo" at "http://repo.spray.io"

enablePlugins(JavaAppPackaging)

libraryDependencies += "io.spray" %% "spray-can" % "1.3.3"
libraryDependencies += "io.spray" %% "spray-routing" % "1.3.3"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.1"

Revolver.settings