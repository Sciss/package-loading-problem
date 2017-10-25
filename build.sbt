name         := "plp"
organization := "de.sciss"
version      := "0.1.0-SNAPSHOT"
scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.6",
  "com.github.scopt"  %% "scopt"      % "3.7.0",
  "de.sciss"          %% "fileutil"   % "1.1.3"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint:-stars-align,_")
