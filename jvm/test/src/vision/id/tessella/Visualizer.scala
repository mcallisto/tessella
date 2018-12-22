package vision.id.tessella

import scala.util.{Failure, Success, Try}

import org.scalatest.exceptions.TestFailedException

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.Graph
import vision.id.graphgephi.Drawable

/** Scalatest support for graph visualization in case of failures.
  */
trait Visualizer extends Drawable {

  final def given(graph: Graph[Int, UnDiEdge])(test: Graph[Int, UnDiEdge] => Unit): Unit = {

    def reThrow(tExc: TestFailedException, secondLine: String) =
      throw tExc.modifyMessage(_.map(testMessage => s"""$testMessage
                                                       |$secondLine
       """.stripMargin))

    Try(test(graph)) match {
      case Failure(tExc: TestFailedException) =>
        makeImage(
          graph,
          path = "out/jvm/test/myGraphErrors/",
          name = (tExc.failedCodeFileName match {
            case Some(fileName) => fileName
            case None           => "failed_test"
          }) + (tExc.failedCodeLineNumber match {
            case Some(number) => "_line" + number.toString
            case None         => ""
          }) + ".png"
        ) match {
          case Success(f) => reThrow(tExc, s"The graph image is available at file://${f.getAbsolutePath}")
          case Failure(e) => reThrow(tExc, s"Graph image generation failed with `${e.getMessage}`.")
        }
      case _ =>
    }
  }
}
