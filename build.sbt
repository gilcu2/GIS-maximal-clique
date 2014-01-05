name := "gis"

version := "1.0"

scalaVersion := "2.10.0"

exportJars := true

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

mainClass in oneJar := Some("app.App")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.0"

libraryDependencies += "com.netflix.rxjava" % "rxjava-scala" % "0.15.0"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)

// Note: These settings are defaults for activator, and reorganize your source directories.
Seq(
  scalaSource in Compile <<= baseDirectory / "src",
  javaSource in Compile <<= baseDirectory / "src",
  sourceDirectory in Compile <<= baseDirectory / "src",
  scalaSource in Test <<= baseDirectory / "test",
  javaSource in Test <<= baseDirectory / "test",
  sourceDirectory in Test <<= baseDirectory / "test",
  resourceDirectory in Compile <<= baseDirectory / "conf"
)
