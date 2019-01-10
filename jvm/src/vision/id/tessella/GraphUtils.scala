package vision.id.tessella

import scala.util.Try

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

trait GraphUtils {

  final implicit class Util(g: Graph[Int, UnDiEdge]) {

    def renumber(start: Int = 1): Graph[Int, UnDiEdge] = {

      val m: Map[Int, Int] = g.nodes.toList.map(_.toOuter).sorted.zip(start until (g.nodes.size + start)).toMap
      val newEdges         = g.edges.map(_.toOuter).map(e => m(e._n(0)) ~ m(e._n(1)))
      val newNodes         = g.nodes.filter(_.degree == 0).map(n => m(n.toOuter))
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

    val ends = g.filter({ case (_, v) => v.size == 1 }).keys.toList

    if (ends.size != 2)
      throw new IllegalArgumentException("1 degrees nodes must be exactly two")

    def toPair(edge: UnDiEdge[Int]): (Int, Int) = (edge._n(0), edge._n(1))

    def loop(es: Set[(Int, Int)], acc: Int): Int = {
      if (es.isEmpty) acc
      else
        es.find({ case (f, s) => f == acc || s == acc }) match {
          case Some((f, s)) if f == acc => loop(es - ((f, s)), s)
          case Some((f, s))             => loop(es - ((f, s)), f)
          case None                     => throw new IllegalArgumentException("disconnected node")
        }
    }

    loop(edges.map(toPair).filterNot({ case (f, s) => f == ends(0) || s == ends(0) }), ends(1))
    (ends(0), ends(1))
  }

}
