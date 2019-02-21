package vision.id.tessella

import scala.annotation.tailrec
import scala.util.Try

import vision.id.tessella.Tessella.Tiling

trait Border extends ListUtils {

  final implicit class XPGraph(tiling: Tiling) {

    private implicit final class XPNode(node: tiling.NodeT) {

      def shortestWithBlocksTo(other: tiling.NodeT, blocks: Set[tiling.NodeT]): Option[tiling.Path] =
        node.withSubgraph(nodes = !blocks.contains(_)) shortestPathTo other

      def shortestPerimeterPath(other: tiling.NodeT, onPerimeter: Boolean): Option[tiling.Path] =
        node.withSubgraph(edges = _.isPerimeter.contains(onPerimeter)) shortestPathTo other

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

      def isPerimeterFlagged: Boolean = edge.isPerimeter.contains(true)
    }

    /**
      * if no perimeter loose edges with outdegree 2 found, this method recursively scan for others
      *
      * @param out  outdegree of one endpoint
      * @param same if true other endpoint same outdegree, if false +1
      * @return
      */
    @tailrec
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

    def setPerimeter: Try[Unit] = Try {

      tiling.edges.foreach(_.isPerimeter = Some(false))

      // start with loose (easy to find) edges
      val es: Iterable[tiling.EdgeT] = tiling.edges.filter(_.isPerimeterLoose) match {
        case none if none.isEmpty => progLooseEdges(3, same = true) // if nothing progressively explore higher degrees
        case some                 => some
      }
      es.foreach(_.isPerimeter = Some(true))

      val perimeterNode: tiling.NodeT = es.toList.safeHead.nodes.toList.safeHead

      def setNextPerimeterEdge(endpoint: tiling.NodeT, other: tiling.NodeT): Unit = {
        // nodes potentially on the perimeter
        val candidates: Set[tiling.NodeT] = endpoint.neighbors.filterNot(_ == other)
        // next is the farthest from other not passing through endpoint
        val next: tiling.NodeT          = candidates.maxBy(_.shortestWithBlocksTo(other, Set(endpoint)).safeGet().size)
        val perimeterEdge: tiling.EdgeT = tiling.get(Side(endpoint.toOuter, next.toOuter))
        perimeterEdge.isPerimeter = Some(true)
      }

      def findNextPerimeterEdge: Unit =
        perimeterNode
          .withSubgraph(edges = _.isPerimeterFlagged)
          .pathUntil(_.edges.count(_.isPerimeterFlagged) < 2) match {
          case None => throw new Error
          case Some(path) =>
            val two = path.nodes.toList.takeRight(2)
            setNextPerimeterEdge(two.safeLast, two.safeHead)
        }

      while (perimeterNode.withSubgraph(edges = _.isPerimeterFlagged).findCycle.isEmpty) {
        findNextPerimeterEdge
      }
    }

    def cleanPerimeter: Unit = tiling.edges.foreach(_.isPerimeter = None)

    def hasPerimeterSet: Boolean = tiling.edges.forall(_.isPerimeter.isDefined)

    /**
      * shortest path between two nodes, inside or outside perimeter
      *
      * @param node1 outer perimeter node
      * @param node2 outer perimeter node
      * @param onPerimeter if following perimeter or non perimeter edges
      * @return
      */
    def getShortestPerimeterPath(node1: Int, node2: Int, onPerimeter: Boolean): Traversable[Side[Int]] =
      (tiling get node1).shortestPerimeterPath(tiling get node2, onPerimeter).safeGet().edges.map(_.toOuter)

  }

}
