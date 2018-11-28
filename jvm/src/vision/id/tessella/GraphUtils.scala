package vision.id.tessella

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

trait GraphUtils {

  final implicit class Util(g: Graph[Int, UnDiEdge]) {

    def renumber(start: Int = 1): Graph[Int, UnDiEdge] = {

      val m: Map[Int, Int] = g.nodes.toList.map(_.toOuter).sorted.zip(start until (g.nodes.size + start)).toMap
      val new_edges        = g.edges.map(e ⇒ m(e._1.toOuter) ~ m(e._2.toOuter))
      val new_nodes        = g.nodes.filter(_.degree == 0).map(n ⇒ m(n.toOuter))
      Graph.from(new_nodes, new_edges)
    }

  }

}
