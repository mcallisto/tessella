package vision.id.tessella

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
  }

  private def removeOrphans(g: Graph[Int, UnDiEdge]): Graph[Int, UnDiEdge] =
    g.nodes.filter(_.degree <= 1) match {
      case none if none.isEmpty => g
      case orphans              => removeOrphans(g -- orphans)
    }

}
