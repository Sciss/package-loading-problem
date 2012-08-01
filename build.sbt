/** Project */
name := "plp"

organization := "org.scala"

version := "0.1"

scalaVersion := "2.9.1"

resolvers ++= Seq(
  "Scala Tools Releases" at "http://scala-tools.org/repo-releases",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.6.1",
  "com.typesafe.akka" % "akka-actor" % "2.0",
  "com.typesafe.akka" % "akka-testkit" % "2.0")

testFrameworks += new TestFramework("org.specs2.runner.SpecsFramework")

scalacOptions ++= Seq("-unchecked", "-deprecation")

// define the statements initially evaluated when entering 'console', 'console-quick', or 'console-project'
initialCommands := """
  import System.{currentTimeMillis => now}
  def time[T](f: => T): T = {
    val start = now
    try { f } finally { println("Elapsed: " + (now - start)/1000.0 + " s") }
  }
"""
