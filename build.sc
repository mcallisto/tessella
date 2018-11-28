import mill._
import mill.define.Sources
import scalalib._
import ammonite.ops._

trait Versioned extends ScalaModule {

  def scalaVersion: T[String] = "2.12.7"

}

trait Common extends Versioned {

  override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg(
    )

  override def sources: Sources = T.sources(
    millSourcePath / "src",
    millSourcePath / up / "shared" / "src"
  )

}

trait Testable extends ScalaModule {

  override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg(
    ivy"org.scalatest::scalatest:3.0.5",
    ivy"org.scalacheck::scalacheck:1.14.0"
  )

}

object jvm extends Common { outer ⇒

  override def unmanagedClasspath: T[Agg[PathRef]] = mill T {
    if (!ammonite.ops.exists(millSourcePath / "lib")) Agg()
    else Agg.from(ammonite.ops.ls(millSourcePath / "lib").map(PathRef(_)))
  }

  override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg(
    ivy"org.scala-graph::graph-core:1.12.5",
    ivy"org.scala-lang.modules::scala-xml:1.1.1",
    ivy"com.lihaoyi::os-lib:0.2.2",
    ivy"com.storm-enroute::scalameter:0.8.2",
    ivy"ch.qos.logback:logback-classic:1.2.3",
    ivy"com.typesafe.scala-logging::scala-logging:3.9.0"
  )

  object test extends outer.Tests with Testable {

    override def unmanagedClasspath: T[Agg[PathRef]] = T {
      super.unmanagedClasspath() ++ outer.unmanagedClasspath()
    }

    def testFrameworks: T[Seq[String]] = Seq("org.scalatest.tools.Framework")

  }

}
