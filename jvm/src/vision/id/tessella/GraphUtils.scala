package vision.id.tessella

import scala.annotation.tailrec
import scala.util.Try

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

trait GraphUtils {

  final implicit class Util(g: Graph[Int, UnDiEdge]) {

    def renumber(start: Int = 1): Graph[Int, UnDiEdge] = {

      val m: Map[Int, Int] = g.nodes.toList.map(_.toOuter).sorted.zip(start until (g.nodes.size + start)).toMap
      val newEdges = g.edges.map(_.nodes.map(_.toOuter) match {
        case n1 :: n2 :: Nil => m(n1) ~ m(n2)
        case _               => throw new Error
      })
      val newNodes = g.nodes.filter(_.degree == 0).map(n => m(n.toOuter))
      Graph.from(newNodes, newEdges)
    }

    def withoutOrphans: Graph[Int, UnDiEdge] = removeOrphans(g)

    /**
      * get max id and unused ones in between
      *
      * @return
      */
    def emptiesMax: (List[Int], Int) = {
      val all = g.nodes.map(_.toOuter).toList
      val m   = all.max
      ((1 until m).toList.diff(all), m)
    }

    def toSides: Set[Side[Int]] = g.edges.map(_.toOuter).map(Side.fromEdge(_)).toSet

  }

  private def removeOrphans(g: Graph[Int, UnDiEdge]): Graph[Int, UnDiEdge] =
    g.nodes.filter(_.degree <= 1) match {
      case none if none.isEmpty => g
      case orphans              => removeOrphans(g -- orphans)
    }

  /**
    * check if all given edges form a straight path
    *
    * @param edges outer edges
    * @return
    */
  def pathEndPoints(edges: Set[UnDiEdge[Int]]): Try[(Int, Int)] = Try {

    val g: Map[Int, List[Int]] = edges.toList.flatMap(_.toList).groupBy(identity)

    if (g.exists({ case (_, v) => v.size > 2 }))
      throw new IllegalArgumentException("3 or more degrees node")

    @tailrec
    def loop(es: Set[UnDiEdge[Int]], acc: Int): Int = {
      if (es.isEmpty) acc
      else
        es.find(_.contains(acc)).map(_.toList) match {
          case Some(f :: s :: Nil) if f == acc => loop(es - (f ~ s), s)
          case Some(f :: s :: Nil)             => loop(es - (f ~ s), f)
          case _                               => throw new IllegalArgumentException("disconnected node")
        }
    }

    g.filter({ case (_, v) => v.size == 1 }).keys.toList match {
      case e1 :: e2 :: Nil =>
        loop(edges.filterNot(_.contains(e1)), e2)
        (e1, e2)
      case _ => throw new IllegalArgumentException("1 degrees nodes must be exactly two")
    }
  }

}
