import sbt._
import sbt.Keys._

object ConcurrentTrieSpellcheckBuild extends Build {

  lazy val ConcurrentTrieSpellcheck = Project(
    id = "ConcurrentTrieSpellcheck",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "ConcurrentTrieSpellcheck",
      organization := "org.agentcoops",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0",
      mainClass := Some("CLISpellCheck"),
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-actor" % "2.1.0",
        "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT"
      )
     )
   )	
}
