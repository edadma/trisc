import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.2"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.0.1"
Global / concurrentRestrictions     := Seq(Tags.limitAll(18))
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
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD", "-W", "30", "30", "-P18"),
  // Exclude full-system integration tests tagged Slow from `sbt test` by default.
  // Run them explicitly with: sbt testSlow  (or `sbt testAll` for everything)
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-l", "io.github.edadma.trisc.Slow"),
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

lazy val svm = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("svm"))
  .settings(commonSettings)
  .settings(
    name := "trisc-svm",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
      "com.lihaoyi" %%% "pprint" % "0.9.0",
    ),
  )
  .dependsOn(asm)
  .jsSettings(jsSettings)
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val cpu = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("cpu"))
  .settings(commonSettings)
  .settings(
    name := "trisc-cpu",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "logger" % "0.0.11",
      "io.github.edadma" %%% "cross_platform" % "0.1.3",
    ),
  )
  .dependsOn(mem)
  .jsSettings(jsSettings)
  .jvmSettings(jvmNativeStubs)
  .nativeSettings(jvmNativeStubs)

lazy val docs = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("docs-cli"))
  .settings(commonSettings)
  .settings(
    name := "trisc-docs",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "markdown" % "0.2.1",
      "io.github.edadma" %%% "highlighter" % "0.0.1",
    ),
  )
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

lazy val sfs = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("sfs"))
  .settings(commonSettings)
  .settings(name := "trisc-sfs")
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
      "io.github.edadma" %%% "toml" % "0.1.0",
    ),
  )
  .dependsOn(cpu, asm, svm, sysl)
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
  .dependsOn(sysl, asm, tof, svm, cpu)
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
    svm.jvm, /* svm.js, svm.native, */
    cpu.jvm, /* cpu.js, cpu.native, */
    docs.jvm, /* docs.js, docs.native, */
    sysl.jvm, /* sysl.js, sysl.native, */
    sfs.jvm, /* sfs.js, sfs.native, */
    triscCli.jvm, /* triscCli.js, triscCli.native, */
    syslCli.jvm, /* syslCli.js, syslCli.native, */
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
    "cpuNative/test" :: "docsNative/test" :: "syslNative/test" :: "sfsNative/test" ::
    "triscCliNative/test" :: "syslCliNative/test" :: state
  },
  Command.command("testJS") { state =>
    "utilsJS/test" :: "memJS/test" :: "tofJS/test" :: "asmJS/test" ::
    "cpuJS/test" :: "docsJS/test" :: "syslJS/test" :: "sfsJS/test" ::
    "triscCliJS/test" :: "syslCliJS/test" :: state
  },
  // Run only tests tagged Slow (full-OS integration tests).
  // Must remove the default -l exclusion first, then add -n inclusion.
  Command.command("testSlow") { state =>
    """set Test / testOptions in ThisBuild -= Tests.Argument(TestFrameworks.ScalaTest, "-l", "io.github.edadma.trisc.Slow")""" ::
    """triscCliJVM/testOnly * -- -n io.github.edadma.trisc.Slow""" ::
    """syslJVM/testOnly * -- -n io.github.edadma.trisc.Slow""" ::
    """set Test / testOptions in ThisBuild += Tests.Argument(TestFrameworks.ScalaTest, "-l", "io.github.edadma.trisc.Slow")""" :: state
  },
  // Run all tests including Slow ones.
  Command.command("testAll") { state =>
    """set Test / testOptions in ThisBuild -= Tests.Argument(TestFrameworks.ScalaTest, "-l", "io.github.edadma.trisc.Slow")""" ::
    "test" ::
    """set Test / testOptions in ThisBuild += Tests.Argument(TestFrameworks.ScalaTest, "-l", "io.github.edadma.trisc.Slow")""" :: state
  },
)
