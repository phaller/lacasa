import sbt._
import Keys._

object LaCasaBuild extends Build {

  lazy val commonSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.7",
    crossVersion := CrossVersion.full,
    version := "0.1.0-SNAPSHOT",
    organization := "org.",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    scalacOptions ++= Seq("-deprecation", "-feature"),
    parallelExecution in Test := false,
    logBuffered := false,
    scalaHome := {
      val scalaHome = System.getProperty("lacasa.scala.home")
      if (scalaHome != null) {
        println(s"Using Scala home directory: $scalaHome")
        Some(file(scalaHome))
      } else None
    }
  )

  lazy val plugin = Project(
    id   = "lacasa",
    base = file("plugin")
  ) settings (
    commonSettings: _*
  ) settings (
    resourceDirectory in Compile <<= baseDirectory(_ / "src" / "main" / "scala" / "lacasa" / "embedded"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-library" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  )

  lazy val usePluginSettings = Seq(
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      System.setProperty("lacasa.plugin.jar", jar.getAbsolutePath)
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
  )

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox")
  ) settings (
    commonSettings ++ usePluginSettings: _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    publishArtifact in Compile := false
  )

}
