scalaVersion := "2.11.5"

scalacOptions += "-deprecation"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += Resolver.sonatypeRepo("snapshots")


mainClass in (Compile, run) := Some("kakadu.Hw")