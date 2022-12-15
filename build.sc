import $ivy.`com.goyeau::mill-scalafix::0.2.11`
import com.goyeau.mill.scalafix.ScalafixModule
import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import os._

val thisPublishVersion = "0.3.0-SNAPSHOT"
val scalaVersions = List(
  "2.13.10"
)
val thisScalaJSVersion = "1.12.0"

val macroParadiseVersion = "2.1.1"
val kindProjectorVersion = "0.13.2"
// val splainVersion = "0.3.4"

// --- core deps ---
// cats libs -- maintain version agreement or whatever
val catsVersion = "2.9.0"
val shapelessVersion = "2.3.10"
val kittensVersion = "3.0.0"
val circeVersion = "0.14.3"
val monocleVersion = "3.1.0"
val simulacrumVersion = "1.0.1"

// for inflection dictionary on jvm; TODO roll my own to be cross-platform and avoid the dep
val trove4jVersion = "3.0.1"

// --- io deps ---
val catsEffectVersion = "3.4.2"
val fs2Version = "3.4.0"
val http4sVersion = "0.23.16"
// js
val scalajsDomVersion = "2.3.0"

// --- datasets deps ---
// val fastparseVersion = "2.3.1"

// --- corenlp deps ---
val corenlpVersion = "3.6.0"

// --- ui deps ---
val scalajsReactVersion = "2.1.1"
val scalacssVersion = "1.0.0"

// val scalatestVersion = "3.0.5"
// val scalacheckVersion = "1.13.5"
// val disciplineVersion = "0.9.0"

trait CommonModule extends CrossScalaModule with ScalafmtModule with ScalafixModule {

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
    "-Yrangepos",
    "-Ymacro-annotations"
  )


  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    // ivy"io.tryp:::splain:$splainVersion",
    ivy"org.typelevel:::kind-projector:$kindProjectorVersion"
  )

  override def ivyDeps = Agg(
    ivy"org.typelevel::cats-core::$catsVersion",
    ivy"com.chuusai::shapeless::$shapelessVersion",
    ivy"org.typelevel::simulacrum:$simulacrumVersion",
    ivy"org.typelevel::kittens::$kittensVersion",
    ivy"dev.optics::monocle-core::$monocleVersion",
    ivy"dev.optics::monocle-macro::$monocleVersion",
    ivy"dev.optics::monocle-generic::$monocleVersion",
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

// NOTE: commented out for the time being to help with version migrations --- isn't critical
// object datasets extends Module {
//   trait DatasetsModule extends CommonPublishModule {
//     def artifactName = "jjm-datasets"
//     def millSourcePath = build.millSourcePath / "jjm-datasets"
//     override def ivyDeps = super.ivyDeps() ++ Agg(
//       ivy"com.lihaoyi::fastparse::$fastparseVersion"
//       // ivy"org.typelevel::cats-effect::$catsEffectVersion",
//       // ivy"co.fs2::fs2-core::$fs2Version"
//     )
//   }

//   class Js(val crossScalaVersion: String) extends DatasetsModule with JsPlatform {
//     def moduleDeps = List(io.js(crossScalaVersion))
//   }
//   object js extends Cross[Js](scalaVersions: _*)

//   class Jvm(val crossScalaVersion: String) extends DatasetsModule with JvmPlatform {
//     def moduleDeps = List(io.jvm(crossScalaVersion))
//   }
//   object jvm extends Cross[Jvm](scalaVersions: _*)
// }

class CoreNLPModule(val crossScalaVersion: String) extends CommonPublishModule with JvmPlatform {
  def artifactName = "jjm-corenlp"
  def millSourcePath = build.millSourcePath / "jjm-corenlp"
  override def sources = T.sources(millSourcePath / "src-jvm")
  def moduleDeps = List(core.jvm(crossScalaVersion))
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"edu.stanford.nlp:stanford-corenlp:$corenlpVersion",
    ivy"edu.stanford.nlp:stanford-corenlp:$corenlpVersion".configure(
      coursier.core.Attributes(`type` = coursier.core.Type(""), classifier = coursier.core.Classifier("models"))
    ),
  )
}
object corenlp extends Cross[CoreNLPModule](scalaVersions: _*)

class UIModule(val crossScalaVersion: String) extends CommonPublishModule with JsPlatform {
  def artifactName = "jjm-ui"
  def millSourcePath = build.millSourcePath / "jjm-ui"
  override def sources = T.sources(millSourcePath / "src-js")
  def moduleDeps = Seq(core.js())

  def ivyDeps = Agg(
    ivy"com.github.japgolly.scalajs-react::core-ext-cats::$scalajsReactVersion",
    ivy"com.github.japgolly.scalajs-react::core-ext-cats_effect::$scalajsReactVersion",
    ivy"com.github.japgolly.scalajs-react::core::$scalajsReactVersion",
    ivy"com.github.japgolly.scalajs-react::extra::$scalajsReactVersion",
    ivy"com.github.japgolly.scalajs-react::extra-ext-monocle3::$scalajsReactVersion",
    ivy"com.github.japgolly.scalacss::core::$scalacssVersion",
    ivy"com.github.japgolly.scalacss::ext-react::$scalacssVersion"
  )
}

object ui extends Cross[UIModule](scalaVersions: _*)
