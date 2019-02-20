package vision.id.tessella

import scala.annotation.tailrec

import vision.id.tessella.Tessella.Tiling

trait Neighbors extends Symmetry with ListUtils {

  final implicit class NGraph(tiling: Tiling) {

    private final implicit class NNode(node: tiling.NodeT) {

      def shortestWithBlocksTo(other: tiling.NodeT, blocks: Set[tiling.NodeT]): Option[tiling.Path] =
        node.withSubgraph(nodes = !blocks.contains(_)) shortestPathTo other

      type nodesL = List[tiling.NodeT]

      type nPaths = List[(tiling.NodeT, nodesL)]

      type nodesPaths = (nodesL, List[nodesL])

      /**
        *
        * @param ns  nodes yet to be added to path
        * @param acc accumulator of nodes and paths found
        * @param b   nodes blocking shortest path
        * @return
        */
      @tailrec
      private def findPathPeri(ns: nodesL, acc: nPaths, b: Set[tiling.NodeT]): nodesPaths = ns match {
        case Nil => acc.reverse.unzip
        case _ =>
          val (accNodes, _)          = acc.unzip //; logger.debug("\nNeighbors ordered so far: " + acc_nodes)
          val lastNode: tiling.NodeT = accNodes.safeHead
          val mapPaths: nPaths       = ns.map(n => (n, n.shortestWithBlocksTo(lastNode, b).safeGet().nodes.toList))
          val (foundNode, pathNodes) = mapPaths.minBy({ case (_, path) => path.size })
          findPathPeri(
            ns.filterNot(_ == foundNode),
            (foundNode, pathNodes) +: acc,
            b ++ Set(lastNode)
          )
      }

      @tailrec
      private def findPathFull(ns: nodesL, acc: nPaths, b: Set[tiling.NodeT]): nodesPaths = ns match {
        case Nil => acc.unzip
        case _ =>
          val (accNodes, _)           = acc.unzip //; logger.debug("\nNeighbors ordered so far: " + acc_nodes)
          val firstNode: tiling.NodeT = accNodes.safeHead
          val lastNode: tiling.NodeT  = accNodes.safeLast
          val blocks: Set[tiling.NodeT] = b ++ (accNodes.tail match {
            case Nil  => Nil
            case some => some.init
          })
          val mapPaths: List[(tiling.NodeT, nodesL, Boolean)] = ns.flatMap(
            n =>
              List((n, n.shortestWithBlocksTo(firstNode, blocks).safeGet().nodes.toList, true),
                   (n, n.shortestWithBlocksTo(lastNode, blocks).safeGet().nodes.toList, false)))
          val (foundNode, pathNodes, isFirst) = mapPaths.minBy({ case (_, path, _) => path.size })
          val accNew: (tiling.NodeT, nodesL)  = (foundNode, pathNodes)
          findPathFull(
            ns.filterNot(_ == foundNode),
            if (isFirst) accNew +: acc else acc :+ accNew,
            b
          )
      }

      def perimeterHood(orderedNodes: nodesL): nodesPaths = {

        def isOnPerimeter(n: tiling.NodeT): Boolean = orderedNodes.contains(n)

        val neighb: nodesL = node.neighbors.toList //; logger.debug("\nNeighbor nodes found: " + neighb)
        // find start without relying on having found all perimeter ordered nodes
        val start: tiling.NodeT = (neighb.filter(isOnPerimeter) match {
          case two @ _ :: _ :: Nil => two
          case more =>
            more
              .combinations(2)
              .maxBy({
                case f :: s :: _ => f.shortestWithBlocksTo(s, Set(node)).safeGet().nodes.size
                case _           => throw new Error
              })
        }) minBy (_.toOuter) //; logger.debug("\nStarting neighbor node chosen: " + start)
        val (nodes, paths): nodesPaths = findPathPeri(
          neighb.filterNot(_ == start),
          List((start, List())),
          Set(node)
        )
        (nodes,
         paths
           .rotate(-1)
           .map({
             case Nil => Nil
             case p   => p.reverse.tail
           }))
      }

      private def reorderFull(nps: nodesPaths): nodesPaths = {
        val (ns, ps)                   = nps
        val indexFirst: Int            = ns.indexOf(ns.minBy(_.toOuter))
        val rotatedNodes: nodesL       = ns.rotate(-indexFirst)
        val rotatedPaths: List[nodesL] = ps.rotate(-indexFirst)
        val next: tiling.NodeT         = rotatedNodes(1)
        val previous: tiling.NodeT     = rotatedNodes(ns.size - 1)
        if (next.toOuter < previous.toOuter)
          (rotatedNodes, rotatedPaths.map(_.tail))
        else
          (rotatedNodes.contraRotate(), rotatedPaths.contraRotate().rotate(-1).map(_.reverse.tail))
      }

      def fullHood: nodesPaths = {
        val neighb: nodesL      = node.neighbors.toList //; logger.debug("\nNeighbor nodes found: " + neighb)
        val start: tiling.NodeT = neighb.minBy(_.toOuter) //; logger.debug("\nStarting neighbor node chosen: " + first)
        val (nodes, paths): (nodesL, List[nodesL]) = findPathFull(
          neighb.filterNot(_ == start),
          List((start, List())),
          Set(node)
        ) //; logger.debug("\nnodes: " + nodes); logger.debug("\npaths: " + paths)
        val first                = nodes.safeHead
        val last                 = nodes.safeLast
        val block                = paths.flatten.toSet ++ nodes.init.tail.flatMap(_.neighbors) - first - last
        val lastPath: nodesL     = first.shortestWithBlocksTo(last, block).safeGet().nodes.toList
        val paths2: List[nodesL] = paths.filterNot(_.isEmpty) :+ lastPath //; logger.debug("\npaths2: " + paths2)
        reorderFull((nodes, paths2.headLastConcat))
      }

      /**
        * if node is full, neighbors ordered from min with direction to lower adjacent
        * if node is not full, ordered from min endpoint to other endpoint
        *
        * @return list of ordered neighbors nodes and ordered path nodes of the underlying p-gon to reach the next one
        */
      def hood(orderedNodes: nodesL): nodesPaths =
        if (orderedNodes.contains(node)) node.perimeterHood(orderedNodes)
        else node.fullHood

    }

    def outerNodeHood(node: Int, orderedNodes: List[Int]): (List[Int], List[List[Int]]) = {
      val (nodes, paths) = (tiling get node).hood(orderedNodes.map(tiling get _))
      (nodes.map(_.toOuter), paths.map(_.map(_.toOuter)))
    }

    /**
      *  try to find one node not yet mapped with at least two neighbors nodes mapped
      */
    def findAddable(mapped: List[Int]): Option[Int] =
      tiling.nodes.toList
        .map(_.toOuter)
        .diff(mapped)
        .find(n => (tiling get n).neighbors.toList.map(_.toOuter).intersect(mapped).lengthCompare(2) >= 0)

    /**
      *  try to find one node already mapped with at least three nodes neighbors
      *  of which at least two already mapped and at least one node not yet mapped
      */
    def findCompletable(mapped: List[Int], nm: NodesMap): Option[Int] =
      mapped.find(node => {
        val neighbors = (tiling get node).neighbors.toList.map(_.toOuter)
        val hasEnoughNeighborsMapped = neighbors.intersect(mapped) match {
          case Nil           => false
          case _ :: Nil      => false
          case f :: s :: Nil => !nm.hasOnSameLine(f, s)
          case _             => true
        }
        hasEnoughNeighborsMapped && neighbors.diff(mapped).nonEmpty
      })

  }
}
