import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.2"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.0.1"
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots
ThisBuild / resolvers += Resolver.sonatypeCentralRepo("releases")

ThisBuild / sonatypeProfileName := "io.github.edadma"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/edadma/trisc"),
    "scm:git@github.com:edadma/trisc.git",
  ),
)
ThisBuild / developers := List(
  Developer(
    id = "edadma",
    name = "Edward A. Maxedon, Sr.",
    email = "edadma@gmail.com",
    url = url("https://github.com/edadma"),
  ),
)

ThisBuild / homepage    := Some(url("https://github.com/edadma/trisc"))
ThisBuild / description := "TRISC - a 16-bit RISC CPU emulator and assembler"

ThisBuild / publishTo := sonatypePublishToBundle.value

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:dynamics",
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
  publishMavenStyle      := true,
  Test / publishArtifact := false,
)

lazy val jsSettings = Seq(
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
  scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Test / scalaJSUseMainModuleInitializer := false,
  Test / scalaJSUseTestModuleInitializer := true,
)

lazy val jvmNativeStubs = Seq(
  libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
)

// --- Sub-projects ---

lazy val utils = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("utils"))
  .settings(commonSettings)
  .settings(
    name := "trisc-utils",
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
  )
  .jsSettings(jsSettings)
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val mem = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("mem"))
  .settings(commonSettings)
  .settings(name := "trisc-mem")
  .dependsOn(utils)
  .jsSettings(jsSettings)
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val tof = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("tof"))
  .settings(commonSettings)
  .settings(
    name := "trisc-tof",
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
  )
  .dependsOn(mem)
  .jsSettings(jsSettings)
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val asm = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("asm"))
  .settings(commonSettings)
  .settings(
    name := "trisc-asm",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
      "com.lihaoyi" %%% "pprint" % "0.9.0",
    ),
  )
  .dependsOn(tof)
  .jsSettings(jsSettings)
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val cpu = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("cpu"))
  .settings(commonSettings)
  .settings(
    name := "trisc-cpu",
    libraryDependencies += "io.github.edadma" %%% "logger" % "0.0.9",
  )
  .dependsOn(mem)
  .jsSettings(jsSettings)
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val docs = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("docs"))
  .settings(commonSettings)
  .settings(name := "trisc-docs")
  .jsSettings(jsSettings)
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val sysl = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("sysl"))
  .settings(commonSettings)
  .settings(
    name := "trisc-sysl",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
      "io.github.edadma" %%% "indentation" % "0.0.1",
    ),
  )
  .dependsOn(docs)
  .jsSettings(jsSettings)
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val triscCli = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("trisc-cli"))
  .settings(commonSettings)
  .settings(
    name := "trisc-cli",
    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "4.1.0",
      "com.lihaoyi" %%% "pprint" % "0.9.0",
    ),
  )
  .dependsOn(cpu, asm, sysl)
  .jsSettings(jsSettings)
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val syslCli = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("sysl-cli"))
  .settings(commonSettings)
  .settings(
    name := "sysl-cli",
    libraryDependencies += "com.github.scopt" %%% "scopt" % "4.1.0",
  )
  .dependsOn(sysl, asm, tof)
  .jsSettings(jsSettings)
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
  )
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val docsCli = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("docs-cli"))
  .settings(commonSettings)
  .settings(
    name := "docs-cli",
    libraryDependencies += "com.github.scopt" %%% "scopt" % "4.1.0",
  )
  .dependsOn(docs)
  .jsSettings(jsSettings)
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
  )
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val ttf = project
  .in(file("ttf/scala"))
  .settings(commonSettings)
  .settings(name := "trisc-ttf")

lazy val fonts = project
  .in(file("fonts"))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    scalacOptions ++= commonScalacOptions,
    name := "trisc-fonts",
    libraryDependencies += "io.github.edadma" %%% "freetype" % "0.0.1",
    nativeConfig ~= { _.withLinkingOptions(Seq("-L/opt/homebrew/lib")) },
  )

lazy val root = project
  .in(file("."))
  .aggregate(
    utils.jvm, /* utils.js, utils.native, */
    mem.jvm, /* mem.js, mem.native, */
    tof.jvm, /* tof.js, tof.native, */
    asm.jvm, /* asm.js, asm.native, */
    cpu.jvm, /* cpu.js, cpu.native, */
    docs.jvm, /* docs.js, docs.native, */
    sysl.jvm, /* sysl.js, sysl.native, */
    triscCli.jvm, /* triscCli.js, triscCli.native, */
    syslCli.jvm, /* syslCli.js, syslCli.native, */
    docsCli.jvm, /* docsCli.js, docsCli.native, */
    ttf,
    /* fonts, */
  )
  .settings(
    name                := "trisc",
    publish / skip      := true,
    publishLocal / skip := true,
  )

// Custom commands for platform-specific test runs
commands ++= Seq(
  Command.command("testNative") { state =>
    "utilsNative/test" :: "memNative/test" :: "tofNative/test" :: "asmNative/test" ::
    "cpuNative/test" :: "docsNative/test" :: "syslNative/test" ::
    "triscCliNative/test" :: "syslCliNative/test" :: "docsCliNative/test" :: state
  },
  Command.command("testJS") { state =>
    "utilsJS/test" :: "memJS/test" :: "tofJS/test" :: "asmJS/test" ::
    "cpuJS/test" :: "docsJS/test" :: "syslJS/test" ::
    "triscCliJS/test" :: "syslCliJS/test" :: "docsCliJS/test" :: state
  },
)
