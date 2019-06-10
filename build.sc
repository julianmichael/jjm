import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import ammonite.ops._

val thisPublishVersion = "0.1.0-SNAPSHOT"
val scalaVersions = List("2.12.8")
val thisScalaJSVersion = "0.6.27"

val macroParadiseVersion = "2.1.0"
val kindProjectorVersion = "0.9.4"
// val splainVersion = "0.3.4"

// cats libs -- maintain version agreement or whatever
val catsVersion = "1.6.1"
val circeVersion = "0.11.1"
val monocleVersion = "1.5.1-cats"
val simulacrumVersion = "0.18.0"
val shapelessVersion = "2.3.3"

// for IO and possibly some generic stuff
val catsEffectVersion = "1.3.1"
// val kittensVersion = "1.1.1"
val fs2Version = "1.0.4"

val corenlpVersion = "3.6.0"

// maybe use for HTTP service hookup thing
// val http4sVersion = "0.20.0-M6"

// val scalatestVersion = "3.0.5"
// val scalacheckVersion = "1.13.5"
// val disciplineVersion = "0.9.0"

// import $file.`scripts-build`.SimpleJSDepsBuild, SimpleJSDepsBuild.SimpleJSDeps
// import $file.`scripts-build`.ScalatexBuild, ScalatexBuild.ScalatexModule

trait CommonModule extends CrossScalaModule with ScalafmtModule {

  def platformSegment: String

  override def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

  override def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Ypartial-unification",
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    // ivy"io.tryp:::splain:$splainVersion",
    ivy"org.scalamacros:::paradise:$macroParadiseVersion",
    ivy"org.spire-math::kind-projector:$kindProjectorVersion",
  )

  override def ivyDeps = Agg(
    ivy"org.typelevel::cats-core::$catsVersion",
    ivy"com.chuusai::shapeless::$shapelessVersion",
    // ivy"org.typelevel::kittens::$kittensVersion",
    ivy"com.github.mpilquist::simulacrum:$simulacrumVersion",
    ivy"com.github.julien-truffaut::monocle-core::$monocleVersion",
    ivy"com.github.julien-truffaut::monocle-macro::$monocleVersion",
    ivy"com.github.julien-truffaut::monocle-generic::$monocleVersion",
    ivy"io.circe::circe-core::$circeVersion",
    ivy"io.circe::circe-parser::$circeVersion",
    ivy"io.circe::circe-generic::$circeVersion"
  )
}

trait JsPlatform extends CommonModule with ScalaJSModule {
  def scalaJSVersion = T(thisScalaJSVersion)
  def platformSegment = "js"
}

trait JvmPlatform extends CommonModule {
  def platformSegment = "jvm"
}

trait CommonPublishModule extends CommonModule with PublishModule {
  // def artifactName = "spacro"
  def publishVersion = thisPublishVersion
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "org.julianmichael",
    url = "https://github.com/julianmichael/jjm",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("julianmichael", "jjm"),
    developers = Seq(
      Developer("julianmichael", "Julian Michael","https://github.com/julianmichael")
    )
  )
}


object core extends Module {
  trait CoreModule extends CommonPublishModule {
    def artifactName = "jjm-core"
    def millSourcePath = build.millSourcePath / "jjm"
  }
  class Jvm(val crossScalaVersion: String) extends CoreModule with JvmPlatform
  object jvm extends Cross[Jvm](scalaVersions: _*)
  class Js(val crossScalaVersion: String) extends CoreModule with JsPlatform
  object js extends Cross[Js](scalaVersions: _*)
}

object io extends Module {
  trait IOModule extends CommonPublishModule {
    def artifactName = "jjm-io"
    def millSourcePath = build.millSourcePath / "jjm-io"
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.typelevel::cats-effect::$catsEffectVersion",
      ivy"co.fs2::fs2-core::$fs2Version",
      ivy"co.fs2::fs2-io::$fs2Version"
    )
  }
  class Jvm(val crossScalaVersion: String) extends IOModule with JvmPlatform {
    def moduleDeps = List(core.jvm(crossScalaVersion))
  }
  object jvm extends Cross[Jvm](scalaVersions: _*)
  // class Js(val crossScalaVersion: String) extends IOModule with JsPlatform {
  //   def moduleDeps = List(core.js(crossScalaVersion))
  // }
  // object js extends Cross[Js](scalaVersions: _*)
}

class CoreNLPModule(val crossScalaVersion: String) extends CommonPublishModule with JvmPlatform {
  def artifactName = "jjm-corenlp"
  def millSourcePath = build.millSourcePath / "jjm-corenlp"
  override def sources = T.sources(millSourcePath / "src-jvm")
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"edu.stanford.nlp:stanford-corenlp:$corenlpVersion",
    ivy"edu.stanford.nlp:stanford-corenlp:$corenlpVersion".configure(
      coursier.core.Attributes(`type` = "", classifier = "models")
    ),
  )
  def moduleDeps = List(core.jvm(crossScalaVersion))
}
object corenlp extends Cross[CoreNLPModule](scalaVersions: _*)

