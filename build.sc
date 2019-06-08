import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import ammonite.ops._

val scalaVersions = List("2.12.8")
val thisScalaJSVersion = "0.6.27"

val macroParadiseVersion = "2.1.0"
val kindProjectorVersion = "0.9.4"
// val splainVersion = "0.3.4"

// cats libs -- maintain version agreement or whatever
val catsVersion = "1.1.0"
val circeVersion = "0.11.1"
val monocleVersion = "1.5.1-cats"
val simulacrumVersion = "0.13.0"
val shapelessVersion = "2.3.3"

// for IO and possibly some generic stuff
// val catsEffectVersion = "1.0.0"
// val kittensVersion = "1.1.1"
// val fs2Version = "1.0.0"

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
    // ivy"org.typelevel::cats-effect::$catsEffectVersion",
    //       ivy"com.chuusai::shapeless::$shapelessVersion",
    //       ivy"org.typelevel::kittens::$kittensVersion",
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


object core extends Module {
  trait CoreModule extends CommonModule { def millSourcePath = build.millSourcePath / "core" }
  class Jvm(val crossScalaVersion: String) extends CoreModule with JvmPlatform
  object jvm extends Cross[Jvm](scalaVersions: _*)
  class Js(val crossScalaVersion: String) extends CoreModule with JsPlatform
  object js extends Cross[Js](scalaVersions: _*)
}

object io extends Module {
  trait IOModule extends CommonModule {
    def millSourcePath = build.millSourcePath / "extra" / "io"
    // TODO fs2 dep
  }
  class Jvm(val crossScalaVersion: String) extends IOModule with JvmPlatform
  object jvm extends Cross[Jvm](scalaVersions: _*)
  // class Js(val crossScalaVersion: String) extends IOModule with JsPlatform
  // object js extends Cross[Js](scalaVersions: _*)
}

class CoreNLPModule(val crossScalaVersion: String) extends CommonModule with JvmPlatform {
  def millSourcePath = build.millSourcePath / "extra" / "corenlp"
  override def sources = T.sources(millSourcePath / "src")
}
object corenlp extends Cross[CoreNLPModule](scalaVersions: _*)

