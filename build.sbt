// [Required] Enable plugin and automatically find def main(args:Array[String]) methods from the classpath
enablePlugins(PackPlugin)

// [Optional] Specify main classes manually
// This example creates `hello` command (target/pack/bin/hello) that calls org.mydomain.Hello#main(Array[String])


lazy val root = (project in file(".")).
  settings(
    name := "oscar-block-model",
    crossScalaVersions := Seq("2.11.8","2.11.12", "2.12.4","2.13.0"),
    scalaVersion := "2.13.0",
    resolvers += "Oscar Releases" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot/",
    libraryDependencies += "oscar" % "oscar-cp_2.13" % "4.1.0-SNAPSHOT" withSources(),
    libraryDependencies += "org.rogach" %% "scallop" % "3.3.1",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test", packMain := Map("cplns" -> "blockmodel.executables.RunLNS")

)
