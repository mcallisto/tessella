package vision.id.tessella

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import vision.id.tessella.Alias.Tiling

import scala.util.Try

trait Border extends ListUtils {

  final implicit class XPGraph(tiling: Tiling) {

    private implicit final class XPNode(node: tiling.NodeT) {

      def shortestWithBlocksTo(other: tiling.NodeT, blocks: Set[tiling.NodeT]): Option[tiling.Path] =
        node.withSubgraph(nodes = !blocks.contains(_)) shortestPathTo other
    }

    implicit final class XPEdge(edge: tiling.EdgeT) {

      /**
        * edge part of the perimeter, with at least one end directing to only 2 other nodes
        * (this condition is NOT necessarily true for all the edges in the perimeter)
        *
        * @return
        */
      def isPerimeterLoose: Boolean = edge.nodes.exists(_.degree == 2)

      def isPerimeterLooseProg(out: Int, same: Boolean = true): Boolean =
        if (same) edge.nodes.forall(_.degree == out)
        else edge.nodes.map(_.degree).sum == out * 2 + 1
    }

    /**
      * if no perimeter loose edges with outdegree 2 found, this method recursively scan for others
      *
      * @param out  outdegree of one endpoint
      * @param same if true other endpoint same outdegree, if false +1
      * @return
      */
    private def progLooseEdges(out: Int, same: Boolean): Iterable[tiling.EdgeT] = {
      val perimeterEdges = tiling.edges.filter(_.isPerimeterLooseProg(out, same))
      if (perimeterEdges.isEmpty) {
        if (same)
          progLooseEdges(out, same = false)
        else
          progLooseEdges(out + 1, same = true)
      } else
        perimeterEdges
    }

    def perimeterEdges: Set[UnDiEdge[Int]] = {

      val periGraph: Graph[Int, UnDiEdge] = {

        // adding edges to perimeter until is completed
        def loop(p: Graph[Int, UnDiEdge]): Graph[Int, UnDiEdge] = {
          if (p.isConnected && p.isCyclic) p
          else {
            // find an edge with endpoint
            val e: p.EdgeT = p.edges.find(_.nodes.exists(_.degree == 1)).safeGet()
            // n1 node of the endpoint
            val (n1, n2): (tiling.NodeT, tiling.NodeT) = {
              val nodes = (tiling get e._n(0), tiling get e._n(1))
              if (e._n(0).degree == 1) nodes else nodes.swap
            }
            // nodes potentially on the perimeter
            val candidates: Set[tiling.NodeT] = n1.diSuccessors.filterNot(_ == n2)
            // next is the farthest from n2 not passing through n1
            val next: tiling.NodeT = candidates.maxBy(_.shortestWithBlocksTo(n2, Set(n1)).safeGet().size)
            loop(p + n1.toOuter ~ next.toOuter)
          }
        }

        // start with loose (easy to find) edges
        val es: Iterable[UnDiEdge[Int]] = (tiling.edges.filter(_.isPerimeterLoose) match {
          case none if none.isEmpty => progLooseEdges(3, same = true) // if nothing progressively explore higher degrees
          case some                 => some
        }).map(_.toOuter)

        loop(Graph.from(edges = es))
      }

      require(periGraph.isCyclic, "perimeter not cyclic")

      require(periGraph.nodes.forall(_.degree == 2), "perimeter not simple: " + periGraph)

      periGraph.edges.map(_.toOuter).toSet
    }

    def cleanPerimeter: Unit = tiling.edges.foreach(_.isPerimeter = None)

    def hasPerimeterSet: Boolean = tiling.edges.forall(_.isPerimeter.isDefined)

    // inefficient, using old method
    def setPerimeter: Try[Unit] = Try {

      val pEdges = perimeterEdges

      tiling.edges.foreach(edge => edge.isPerimeter = Some(pEdges.contains(edge.toOuter)))

    }

  }

}
