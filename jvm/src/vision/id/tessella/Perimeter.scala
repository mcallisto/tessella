package vision.id.tessella

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

trait Perimeter extends ListUtils {

  final implicit class XPGraph(graph: Graph[Int, UnDiEdge]) {

    private implicit final class XPNode(node: graph.NodeT) {

      def shortestWithBlocksTo(other: graph.NodeT, blocks: Set[graph.NodeT]): Option[graph.Path] =
        node.withSubgraph(nodes = !blocks.contains(_)) shortestPathTo other

      def isMinimal: Boolean = node.degree == 2
    }

    implicit final class XPEdge(edge: graph.EdgeT) {

      /**
        * edge part of the perimeter, with at least one end directing to only 2 other nodes
        * (this condition is NOT necessarily true for all the edges in the perimeter)
        *
        * @return
        */
      def isPerimeterLoose: Boolean = edge._n(0).isMinimal || edge._n(1).isMinimal

      def isPerimeterLooseProg(out: Int, same: Boolean = true): Boolean = {
        val (d1, d2) = (edge._n(0).degree, edge._n(1).degree)
        if (same) d1 == out && d2 == out
        else (d1 == out && d2 == out + 1) || (d1 == out + 1 && d2 == out)
      }
    }

    /**
      * if no perimeter loose edges with outdegree 2 found, this method recursively scan for others
      *
      * @param out  outdegree of one endpoint
      * @param same if true other endpoint same outdegree, if false +1
      * @return
      */
    private def progLooseEdges(out: Int, same: Boolean): Iterable[graph.EdgeT] = {
      val perimeterEdges = graph.edges.filter(_.isPerimeterLooseProg(out, same))
      if (perimeterEdges.isEmpty) {
        if (same)
          progLooseEdges(out, same = false)
        else
          progLooseEdges(out + 1, same = true)
      } else
        perimeterEdges
    }

    private def perimeter: graph.Path = {

      val periGraph: Graph[Int, UnDiEdge] = {

        // adding edges to perimeter until is completed
        def loop(p: Graph[Int, UnDiEdge]): Graph[Int, UnDiEdge] = {
          if (p.isConnected && p.isCyclic) p
          else {
            // find an edge with endpoint
            val e: p.EdgeT = p.edges.find(_.nodes.exists(_.degree == 1)).safeGet()
            // n1 node of the endpoint
            val (n1, n2): (graph.NodeT, graph.NodeT) = {
              val nodes = (graph get e._n(0), graph get e._n(1))
              if (e._n(0).degree == 1) nodes else nodes.swap
            }
            // nodes potentially on the perimeter
            val candidates: Set[graph.NodeT] = n1.diSuccessors.filterNot(_ == n2)
            // next is the farthest from n2 not passing through n1
            val next: graph.NodeT = candidates.maxBy(_.shortestWithBlocksTo(n2, Set(n1)).safeGet().size)
            loop(p + n1.toOuter ~ next.toOuter)
          }
        }

        // start with loose (easy to find) edges
        val es: Iterable[UnDiEdge[Int]] = (graph.edges.filter(_.isPerimeterLoose) match {
          case none if none.isEmpty => progLooseEdges(3, same = true) // if nothing progressively explore higher degrees
          case some                 => some
        }).map(_.toOuter)

        loop(Graph.from(edges = es))
      }

      require(periGraph.isCyclic, "perimeter not cyclic")

      require(periGraph.nodes.forall(_.degree == 2), "perimeter not simple: " + periGraph)

      val nodeOrdering = periGraph.NodeOrdering(Ordering.Int.compare(_, _))

      /**
        * ordered from lowest node in the direction where the lower neighbor is found
        */
      val orderedCycle: periGraph.Cycle =
        periGraph.nodes.minBy(_.toOuter).withOrdering(nodeOrdering).findCycle.safeGet()

      val ns = orderedCycle.nodes.toList

      ns.tail
        .foldLeft(graph.newPathBuilder(graph get ns.safeHead.toOuter))((pb, n) => pb += (graph get n.toOuter))
        .result()
    }

    def perimeterNodesEdges: (List[Int], List[UnDiEdge[Int]]) = {
      val p  = perimeter
      val ns = p.nodes.toList.map(_.toOuter)
      val es = p.edges.toList.map(_.toOuter)
      val h  = ns.safeHead
      (ns :+ h, es :+ (ns.safeLast ~ h))
    }

  }

}
