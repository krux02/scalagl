name := "scalagl"

organization := "ad"

version := "0.1.0"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.scala-lang" % "scala-reflect" % "2.10.1",
  "org.simplex3d" %% "simplex3d-math-double" % "2.4.7",
  "org.simplex3d" %% "simplex3d-math-float" % "2.4.7"
)

seq( LWJGLPlugin.lwjglSettings: _*)

lwjgl.version := "2.9.0"

scalacOptions ++= Seq(
  "-unchecked", 
  "-deprecation", 
  "-feature", 
  "-Yinline-warnings", 
  "-language:_"
)

javaOptions in run ++= Seq(
  "-Xmx512m",
  "-Dorg.lwjgl.util.Debug=true"
)
