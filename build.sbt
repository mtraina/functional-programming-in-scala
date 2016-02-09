name := "fpis"
organization := "com.mtraina"
version := "0.0.2"
scalaVersion := "2.11.7"

libraryDependencies ++= {
  val scalaTestV  = "2.2.6"
  Seq(
    "org.scalatest" %%  "scalatest" % scalaTestV  % "test"
  )
}