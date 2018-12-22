package vision.id.tessella

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge

trait Neighbors extends Symmetry with ListUtils {

  final implicit class NGraph(graph: Graph[Int, UnDiEdge]) {

    private final implicit class NNode(node: graph.NodeT) {

      def shortestWithBlocksTo(other: graph.NodeT, blocks: Set[graph.NodeT]): Option[graph.Path] =
        node.withSubgraph(nodes = !blocks.contains(_)) shortestPathTo other

      type nodesL = List[graph.NodeT]

      type nPaths = List[(graph.NodeT, nodesL)]

      /**
        *
        * @param ns  nodes yet to be added to path
        * @param acc accumulator of nodes and paths found
        * @param b   nodes blocking shortest path
        * @return
        */
      private def findPathPeri(ns: nodesL, acc: nPaths, b: Set[graph.NodeT]): nPaths = ns match {
        case Nil => acc
        case _ =>
          val (accNodes, _)          = acc.unzip //; logger.debug("\nNeighbors ordered so far: " + acc_nodes)
          val lastNode: graph.NodeT  = accNodes.safeHead
          val mapPaths: nPaths       = ns.map(n => (n, n.shortestWithBlocksTo(lastNode, b).safeGet().nodes.toList))
          val (foundNode, pathNodes) = mapPaths.minBy({ case (_, path) => path.size })
          findPathPeri(
            ns.filterNot(_ == foundNode),
            (foundNode, pathNodes) +: acc,
            b ++ Set(lastNode)
          )
      }

      private def findPathFull(ns: nodesL, acc: nPaths, b: Set[graph.NodeT]): nPaths = ns match {
        case Nil => acc
        case _ =>
          val (accNodes, _)          = acc.unzip //; logger.debug("\nNeighbors ordered so far: " + acc_nodes)
          val firstNode: graph.NodeT = accNodes.safeHead
          val lastNode: graph.NodeT  = accNodes.safeLast
          val blocks: Set[graph.NodeT] = b ++ (accNodes.tail match {
            case Nil  => Nil
            case some => some.init
          })
          val mapPaths: List[(graph.NodeT, nodesL, Boolean)] = ns.flatMap(
            n =>
              List((n, n.shortestWithBlocksTo(firstNode, blocks).safeGet().nodes.toList, true),
                   (n, n.shortestWithBlocksTo(lastNode, blocks).safeGet().nodes.toList, false)))
          val (foundNode, pathNodes, isFirst) = mapPaths.minBy({ case (_, path, _) => path.size })
          val accNew: (graph.NodeT, nodesL)   = (foundNode, pathNodes)
          findPathFull(
            ns.filterNot(_ == foundNode),
            if (isFirst) accNew +: acc else acc :+ accNew,
            b
          )
      }

      def perimeterHood(orderedNodes: List[graph.NodeT]): nPaths = {

        def isOnPerimeter(n: graph.NodeT): Boolean = orderedNodes.contains(n)

        val neighb: nodesL = node.neighbors.toList //; logger.debug("\nNeighbor nodes found: " + neighb)
        // find start without relying on having found all perimeter ordered nodes
        val start: graph.NodeT = (neighb.filter(isOnPerimeter) match {
          case two @ _ :: _ :: Nil => two
          case more =>
            more
              .combinations(2)
              .maxBy({
                case f :: s :: _ => f.shortestWithBlocksTo(s, Set(node)).safeGet().nodes.size
                case _           => throw new Error
              })
        }) minBy (_.toOuter) //; logger.debug("\nStarting neighbor node chosen: " + start)
        val (nodes, paths): (nodesL, List[nodesL]) = findPathPeri(
          neighb.filterNot(_ == start),
          List((start, List())),
          Set(node)
        ).reverse.unzip
        nodes.zip(
          paths
            .rotate(-1)
            .map({
              case Nil => Nil
              case p   => p.reverse.tail
            }))
      }

      private def reorderFull(ps: nPaths): nPaths = {
        val first: (graph.NodeT, nodesL)         = ps.minBy({ case (n, _) => n.toOuter })
        val indexFirst: Int                      = ps.indexOf(first)
        val rotated: nPaths                      = ps.rotate(-indexFirst)
        val (next, _): (graph.NodeT, nodesL)     = rotated(1)
        val (previous, _): (graph.NodeT, nodesL) = rotated(ps.size - 1)
        if (next.toOuter < previous.toOuter)
          rotated
        else {
          val (nodes, paths) = rotated.contraRotate().unzip
          nodes.zip(paths.rotate(-1).map(_.reverse))
        }
      }

      def fullHood: nPaths = {
        val neighb: nodesL     = node.neighbors.toList //; logger.debug("\nNeighbor nodes found: " + neighb)
        val start: graph.NodeT = neighb.minBy(_.toOuter) //; logger.debug("\nStarting neighbor node chosen: " + first)
        val (nodes, paths): (nodesL, List[nodesL]) = findPathFull(
          neighb.filterNot(_ == start),
          List((start, List())),
          Set(node)
        ).unzip //; logger.debug("\nnodes: " + nodes); logger.debug("\npaths: " + paths)
        val first                = nodes.safeHead
        val last                 = nodes.safeLast
        val block                = paths.flatten.toSet ++ nodes.init.tail.flatMap(_.neighbors) - first - last
        val lastPath: nodesL     = first.shortestWithBlocksTo(last, block).safeGet().nodes.toList
        val paths2: List[nodesL] = paths.filterNot(_.isEmpty) :+ lastPath //; logger.debug("\npaths2: " + paths2)
        reorderFull(nodes.zip(paths2.headLastConcat)).map({ case (n, path) => (n, path.tail) })
      }

      /**
        * if node is full, neighbors ordered from min with direction to lower adjacent
        * if node is not full, ordered from min endpoint to other endpoint
        *
        * @return list of ordered neighbors nodes and ordered path nodes of the underlying p-gon to reach the next one
        */
      def hood(orderedNodes: List[graph.NodeT]): nPaths =
        if (orderedNodes.contains(node)) node.perimeterHood(orderedNodes)
        else node.fullHood

    }

    def outerNodeHood(node: Int, orderedNodes: List[Int]): List[(Int, List[Int])] = {
      val (nodes, paths) = (graph get node).hood(orderedNodes.map(graph get _)).unzip
      nodes.map(_.toOuter).zip(paths.map(_.map(_.toOuter)))
    }
  }
}
