name := "fpis"
organization := "com.mtraina"
version := "0.1.0"
scalaVersion := "2.11.7"

libraryDependencies ++= {
  val scalaTestV  = "2.2.6"
  val scalaCheckV  = "1.13.4"
  Seq(
    "org.scalatest" %%  "scalatest" % scalaTestV  % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckV
  )
}