name := "funcProg"

version := "1.0"

scalaVersion := "2.12.2"

fork in Test := true

val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % Test,
  "org.mockito" % "mockito-all" % "1.10.19" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
)

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.4"

libraryDependencies ++= Seq(
) ++ testDependencies


