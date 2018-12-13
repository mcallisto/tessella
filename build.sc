import mill._
import mill.define.Sources
import scalalib._
import ammonite.ops._
import coursier.maven.MavenRepository

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

  override def repositories: Seq[coursier.Repository] = super.repositories ++ Seq(
    MavenRepository("http://bits.netbeans.org/nexus/content/groups/netbeans/"),
    MavenRepository("https://raw.github.com/gephi/gephi/mvn-thirdparty-repo/")
  )

  override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg(
    ivy"org.scalatest::scalatest:3.0.5",
    ivy"org.scalacheck::scalacheck:1.14.0",
    ivy"org.gephi:gephi-toolkit:0.9.2"
  )

}

object jvm extends Common { outer ⇒

  override def unmanagedClasspath: T[Agg[PathRef]] = mill T {
    if (!ammonite.ops.exists(millSourcePath / "lib")) Agg()
    else Agg.from(ammonite.ops.ls(millSourcePath / "lib").map(PathRef(_)))
  }

  override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg(
    ivy"org.scala-graph::graph-core:1.12.5",
    ivy"org.scala-graph::graph-constrained:1.12.5",
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

    def one(args: String*) = T.command {
      val z = args.map("vision.id.tessella." + _)
      super.runMain("org.scalatest.run", z: _*)
    }
  }

}
