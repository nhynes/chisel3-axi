name := "chisel3-axi"
version := "0.1.0"

scalaVersion := "2.12.7"
scalacOptions ++= Seq(
  // Enable the compile option switch to support anonymous Bundle defs:
  // https://github.com/scala/bug/issues/10047
  "-Xsource:2.11",
  "-language:reflectiveCalls",
  "-language:implicitConversions",
  "-deprecation",
  "-Xlint",
  "-Ywarn-unused",
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
)

libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % "3.2-SNAPSHOT",
  "edu.berkeley.cs" %% "chisel-iotesters" % "1.2.5+",
)
