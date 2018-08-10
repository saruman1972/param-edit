name := "param-edit"

version := "0.2"

scalaVersion := "2.10.7"

scalacOptions ++= Seq("-deprecation")

javaOptions += "-Xmx6G"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "scala sbt Repository" at "http://dl.bintray.com/sbt/sbt-plugin-releases/"

libraryDependencies <+= scalaVersion { sv =>
  "org.scala-lang" % "scala-swing" % sv
}

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)

libraryDependencies += "org.swinglabs" % "swingx" % "1.6" % "compile" withSources()

libraryDependencies += "com.github.benhutchison" % "scalaswingcontrib" % "1.5"

libraryDependencies += "jline" % "jline" % "1.0"

libraryDependencies += "org.squeryl" %% "squeryl" % "0.9.5-6"

libraryDependencies += "org.apache.poi" % "poi" % "3.9"

libraryDependencies += "org.apache.poi" % "poi-ooxml" % "3.9"

libraryDependencies += "org.apache.poi" % "ooxml-schemas" % "1.1"

retrieveManaged := true

initialCommands in console := """
  import scala.swing._
  import Swing._
"""

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

mainClass in oneJar := Some("com.allinfinance.tools.param.util.ParamUtil")

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"


