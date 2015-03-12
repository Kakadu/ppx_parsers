import sbt._
import sbt.Keys._

object HelloBuild extends Build {

    val sampleKeyA = SettingKey[String]("sample-a", "demo key A")
    val sampleKeyB = SettingKey[String]("sample-b", "demo key B")
    val sampleKeyC = SettingKey[String]("sample-c", "demo key C")
    val sampleKeyD = SettingKey[String]("sample-d", "demo key D")

    def commonSettings = Seq(
      scalaVersion := "2.11.5",
      resolvers ++= Seq("Sonatype Releases" at "http://oss.sonatype.org/content/repositories/snapshots"),

      libraryDependencies ++=  Seq(
        "org.scala-lang" % "scala-compiler"  % scalaVersion.value % "provided",
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
        "com.github.begeric" % "fastparsers_2.11" % "0.1-SNAPSHOT"
      )
    )
    def macroSettings = Seq(
      scalacOptions := Seq()
        //++ Seq("-Xprint:parser")      // doesn't expand macroses
        //++ Seq("-Ymacro-debug-lite")
    )

    override lazy val settings = super.settings ++
        Seq(sampleKeyA := "A: in Build.settings in Build.scala")

    lazy val root = Project(
        id = "hello",
        base = file("hello1"),
        settings = commonSettings
          ++ macroSettings
          ++ Seq(
            libraryDependencies += "org.scalatest"  % "scalatest_2.11"   % "2.2.1" % "test",
            libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
            libraryDependencies += "com.storm-enroute" % "scalameter_2.11" % "0.6" % "test",
            testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
          )
          ++ Seq(sampleKeyB := "B: in the root project settings in Build.scala")
        )

  lazy val test1 = Project(
      id = "test1",
      base = file("test1"),
      dependencies = Seq(root % "compile->compile;test->test"),
      settings = Seq()
        ++ commonSettings
        //++ Seq(mainClass := Some("Test1Runner"))
        ++ Seq (
          libraryDependencies += "org.scalatest"  % "scalatest_2.11"   % "2.2.1" % "test",
          libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
          libraryDependencies += "com.storm-enroute" % "scalameter_2.11" % "0.6" % "test",

          testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
        )
    )
}


