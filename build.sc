import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import ammonite.ops._

val thisPublishVersion = "0.2.0-SNAPSHOT"
val scalaVersions = List(
  "2.12.13",
  "2.13.4"
)
val thisScalaJSVersion = "1.4.0"
// val thisScalaJSVersion = "0.6.33"

val macroParadiseVersion = "2.1.1"
val kindProjectorVersion = "0.11.3"
// val splainVersion = "0.3.4"

// --- core deps ---
// cats libs -- maintain version agreement or whatever
val catsVersion = "2.3.1"
val shapelessVersion = "2.3.3"
val kittensVersion = "2.2.1"
val circeVersion = "0.13.0"
val monocleVersion = "2.0.3"
val simulacrumVersion = "1.0.0"

// for inflection dictionary; TODO roll my own to be cross-platform and avoid the dep
val trove4jVersion = "3.0.1"

// --- io deps ---
val catsEffectVersion = "2.3.1"
val fs2Version = "2.5.0"
val http4sVersion = "0.21.18"

// js
val scalajsDomVersion = "1.1.0"

// --- corenlp deps ---
val corenlpVersion = "3.6.0"

// val scalatestVersion = "3.0.5"
// val scalacheckVersion = "1.13.5"
// val disciplineVersion = "0.9.0"

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
    "-language:higherKinds"
  ) ++ (
    if(crossScalaVersion.startsWith("2.12")) {
      Seq("-Ypartial-unification")
    } else Seq("-Ymacro-annotations")
  )


  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    // ivy"io.tryp:::splain:$splainVersion",
    ivy"org.typelevel:::kind-projector:$kindProjectorVersion"
  ) ++ (
    if(crossScalaVersion.startsWith("2.12")) {
      Agg(ivy"org.scalamacros:::paradise:$macroParadiseVersion")
    } else Agg()
  )

  override def ivyDeps = Agg(
    ivy"org.typelevel::cats-core::$catsVersion",
    ivy"com.chuusai::shapeless::$shapelessVersion",
    ivy"org.typelevel::simulacrum:$simulacrumVersion",
    ivy"org.typelevel::kittens::$kittensVersion",
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
  class Jvm(val crossScalaVersion: String) extends CoreModule with JvmPlatform {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"net.sf.trove4j:trove4j:$trove4jVersion"
    )
  }
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
      ivy"co.fs2::fs2-core::$fs2Version"
    )
  }
  class Jvm(val crossScalaVersion: String) extends IOModule with JvmPlatform {
    def moduleDeps = List(core.jvm(crossScalaVersion))
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"co.fs2::fs2-io::$fs2Version",
      ivy"org.http4s::http4s-client::$http4sVersion",
      ivy"org.http4s::http4s-dsl::$http4sVersion",
      ivy"org.http4s::http4s-blaze-server::$http4sVersion",
      ivy"org.http4s::http4s-circe::$http4sVersion"
    )
  }
  object jvm extends Cross[Jvm](scalaVersions: _*)
  class Js(val crossScalaVersion: String) extends IOModule with JsPlatform {
    def moduleDeps = List(core.js(crossScalaVersion))
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scala-js::scalajs-dom::$scalajsDomVersion"
    )
  }
  object js extends Cross[Js](scalaVersions: _*)
}

class CoreNLPModule(val crossScalaVersion: String) extends CommonPublishModule with JvmPlatform {
  def artifactName = "jjm-corenlp"
  def millSourcePath = build.millSourcePath / "jjm-corenlp"
  override def sources = T.sources(millSourcePath / "src-jvm")
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"edu.stanford.nlp:stanford-corenlp:$corenlpVersion",
    ivy"edu.stanford.nlp:stanford-corenlp:$corenlpVersion".configure(
      coursier.core.Attributes(`type` = coursier.core.Type(""), classifier = coursier.core.Classifier("models"))
    ),
  )
  def moduleDeps = List(core.jvm(crossScalaVersion))
}
object corenlp extends Cross[CoreNLPModule](scalaVersions: _*)

