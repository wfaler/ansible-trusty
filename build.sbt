resolvers ++= Seq(Resolver.url("sbt-plugin-releases",
new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns))

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.2")

addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.1.2")