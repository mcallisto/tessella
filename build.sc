import mill._
import scalalib._
import publish._
import coursier.maven.MavenRepository

trait Packageable {

  val organization: String = "vision.id"

  val name: String = "tessella"

}

trait Versioned extends ScalaModule with PublishModule with Packageable {

  val githubName: String = name

  def scalaVersion: T[String] = "2.12.8"

  def publishVersion: T[String] = "0.1.3"

  override def artifactName: T[String] = name

  def pomSettings: T[PomSettings] = PomSettings(
    description =
      "Tilings by regular polygons, helps working with finite unit-regular-polygon tessellations of a flat surface",
    organization = organization,
    url = "https://github.com/mcallisto/" + githubName,
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("mcallisto", githubName),
    developers = Seq(
      Developer("mcallisto", "Mario Càllisto", "https://github.com/mcallisto")
    )
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
    ivy"vision.id::graphgephi:0.1.0"
  )

}

object jvm extends Versioned { outer ⇒

  override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg(
    ivy"org.scala-graph::graph-core:1.12.5",
    ivy"org.scala-graph::graph-constrained:1.12.5",
    ivy"org.scala-lang.modules::scala-xml:1.1.1",
    ivy"com.lihaoyi::os-lib:0.2.6",
    ivy"com.storm-enroute::scalameter:0.8.2",
    ivy"ch.qos.logback:logback-classic:1.2.3",
    ivy"com.typesafe.scala-logging::scala-logging:3.9.0"
  )

  object test extends outer.Tests with Testable with Packageable {

    def testFrameworks: T[Seq[String]] = Seq("org.scalatest.tools.Framework")

    def one(args: String*) = T.command {
      super.runMain("org.scalatest.run", args.map(List(organization, name, _).mkString(".")): _*)
    }
  }

}
