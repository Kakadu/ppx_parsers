scalaVersion := "2.11.5"

scalacOptions += "-deprecation"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "INRIA" at "https://maven.inria.fr/artifactory/repo"

resolvers += Resolver.sonatypeRepo("snapshots")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")

mainClass in (Compile, run) := Some("kakadu.Hw")