ThisBuild / scalaVersion := "3.3.0"


lazy val root = project.in(file("."))
  .aggregate(playground.js, playground.jvm)
  .settings(
    publish := {},
    publishLocal := {},
  )

lazy val playground = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "CZPlayground",
    version := "0.1",
  ).jvmSettings(
//    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"
  ).jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.2.0",
    libraryDependencies += "be.adamv" %%% "cz2" % "0.2.9",
    scalaJSUseMainModuleInitializer := true,
  )