package vision.id.tessella

import scala.util.{Failure, Success}

import org.scalatest.FlatSpec

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._ // shortcuts

class graphUtilsTest extends FlatSpec with GraphUtils {

  "A graph" can "have its node reordered starting from one" in {

    val g = Graph(10 ~ 20, 20 ~ 30, 30 ~ 10, 10 ~ 40, 40 ~ 50, 50 ~ 20, 60)

    assert(g.renumber() === Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 1 ~ 4, 4 ~ 5, 5 ~ 2, 6))
    assert(g.renumber(11) === Graph(11 ~ 12, 12 ~ 13, 13 ~ 11, 11 ~ 14, 14 ~ 15, 15 ~ 12, 16))
  }

  val noPath: Set[UnDiEdge[Int]]       = Set(10 ~ 20, 20 ~ 30, 30 ~ 10, 10 ~ 40, 40 ~ 50, 50 ~ 20)
  val cycle: Set[UnDiEdge[Int]]        = Set(1 ~ 2, 3 ~ 4, 5 ~ 4, 3 ~ 2, 1 ~ 5)
  val disconnected: Set[UnDiEdge[Int]] = Set(1 ~ 2, 3 ~ 4, 5 ~ 4, 3 ~ 2, 6 ~ 7, 7 ~ 8, 8 ~ 6)

  "A list of edges" can "be checked if they form a path" in {
    assert(pathEndPoints(noPath) match {
      case Failure(e) => e.getMessage == "3 or more degrees node"
      case _          => false
    })
    assert(pathEndPoints(cycle) match {
      case Failure(e) => e.getMessage == "1 degrees nodes must be exactly two"
      case _          => false
    })
    assert(pathEndPoints(disconnected) match {
      case Failure(e) => e.getMessage == "disconnected node"
      case _          => false
    })
    val yes = Set(1 ~ 2, 3 ~ 4, 5 ~ 4, 3 ~ 2)
    assert(pathEndPoints(yes) === Success(5, 1))
    val yes2 = Set(6 ~ 1, 4 ~ 5, 5 ~ 6)
    assert(pathEndPoints(yes2) === Success(4, 1))
  }

}
