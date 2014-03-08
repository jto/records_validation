name := "records_validation"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "com.chuusai" % "shapeless" % "2.0.0-SNAPSHOT" cross CrossVersion.full changing()
)

play.Project.playScalaSettings
