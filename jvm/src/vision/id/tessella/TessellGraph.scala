package vision.id.tessella

import scala.util.Try

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import com.typesafe.scalalogging.Logger

import vision.id.tessella.Cartesian2D._
import vision.id.tessella.Polar.{PointPolar, UnitSimplePgon}
import vision.id.tessella.Tau.τ

final class TessellGraph(val graph: Graph[Int, UnDiEdge])
    extends Symmetry
    with MathUtils
    with DistinctUtils[Polygon]
    with ListUtils {

  protected type T = TessellGraph

  val logger = Logger("TESSELL")

  require(graph.isConnected, "tessell not connected")

  require(graph.nodes.forall(_.toOuter > 0), "nodes with non positive ids: " + graph.nodes.filterNot(_.toOuter > 0))

  require(graph.nodes.forall(_.isRegular), "nodes with wrong number of edges: " + graph.nodes.filterNot(_.isRegular))

  implicit final class ExtNode(node: graph.NodeT) {

    def shortestWithBlocksTo(n: graph.NodeT, blocks: Set[graph.NodeT]): Option[graph.Path] =
      node.withSubgraph(nodes = !blocks.contains(_)) shortestPathTo n

    /**
      * a node (vertex) can be shared by max 6 reg p-gons (triangles)
      * and it is shared by at least 2 sides of a reg p-gon
      *
      * @return
      */
    def isRegular: Boolean = {
      val d: Int = node.degree
      d >= 2 && d <= 6
    }

    def isMinimal: Boolean = node.degree == 2

    /**
      * node part of the perimeter
      * (this condition is NOT necessarily true for all the nodes in the perimeter)
      *
      * @return
      */
    def isPerimeterLoose: Boolean = isMinimal || node.neighbors.exists(_.isMinimal)

    /**
      * @note a node not full is on the perimeter
      * @return
      */
    def isPerimeter: Boolean = orderedNodes.contains(node)

    def isPerimeterJunction: Boolean = isPerimeter && node.degree > 2

    def isOneNodeFromPeri: Boolean = node.neighbors.exists(_.isPerimeter)

    def isTwoOrLessNodesFromPeri: Boolean =
      (node.neighbors.foldLeft(Set(): Set[graph.NodeT])(_ ++ _.neighbors) - node).exists(_.isPerimeter)

    // ----------------- node neighbors -------------------

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
      case Nil ⇒ acc
      case _ ⇒
        val (acc_nodes, _)         = acc.unzip //; logger.debug("\nNeighbors ordered so far: " + acc_nodes)
        val lastNode: graph.NodeT  = acc_nodes.head
        val mapPaths: nPaths       = ns.map(n ⇒ (n, n.shortestWithBlocksTo(lastNode, b).get.nodes.toList))
        val (foundNode, pathNodes) = mapPaths.minBy({ case (_, path) ⇒ path.size })
        findPathPeri(
          ns.filterNot(_ == foundNode),
          (foundNode, pathNodes) +: acc,
          b ++ Set(lastNode)
        )
    }

    private def findPathFull(ns: nodesL, acc: nPaths, b: Set[graph.NodeT]): nPaths = ns match {
      case Nil ⇒ acc
      case _ ⇒
        val (acc_nodes, _)         = acc.unzip //; logger.debug("\nNeighbors ordered so far: " + acc_nodes)
        val firstNode: graph.NodeT = acc_nodes.head
        val lastNode: graph.NodeT  = acc_nodes.last
        val blocks: Set[graph.NodeT] = b ++ (acc_nodes.tail match {
          case Nil  ⇒ Nil
          case some ⇒ some.init
        })
        val mapPaths: List[(graph.NodeT, nodesL, Boolean)] = ns.flatMap(
          n ⇒
            List((n, n.shortestWithBlocksTo(firstNode, blocks).get.nodes.toList, true),
                 (n, n.shortestWithBlocksTo(lastNode, blocks).get.nodes.toList, false)))
        val (foundNode, pathNodes, isFirst) = mapPaths.minBy({ case (_, path, _) ⇒ path.size })
        val acc_new: (graph.NodeT, nodesL)  = (foundNode, pathNodes)
        findPathFull(
          ns.filterNot(_ == foundNode),
          if (isFirst) acc_new +: acc else acc :+ acc_new,
          b
        )
    }

    def perimeterNodeNeighbors: nPaths = {
      val neighb: nodesL = node.neighbors.toList //; logger.debug("\nNeighbor nodes found: " + neighb)
      // find start without relying on having found all perimeter ordered nodes
      val start: graph.NodeT = (neighb.filter(_.isPerimeter) match {
        case two @ _ :: _ :: Nil ⇒ two
        case more ⇒
          more
            .combinations(2)
            .maxBy({
              case f :: s :: _ ⇒ f.shortestWithBlocksTo(s, Set(node)).get.nodes.size
              case _           ⇒ throw new Error
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
            case Nil ⇒ Nil
            case p   ⇒ p.reverse.tail
          }))
    }

    private def reorderFull(ps: nPaths): nPaths = {
      val first: (graph.NodeT, nodesL)    = ps.minBy({ case (n, _) ⇒ n.toOuter })
      val indexFirst: Int                 = ps.indexOf(first)
      val rotated: nPaths                 = ps.rotate(-indexFirst)
      val next: (graph.NodeT, nodesL)     = rotated(1)
      val previous: (graph.NodeT, nodesL) = rotated(ps.size - 1)
      if (next._1.toOuter < previous._1.toOuter)
        rotated
      else {
        val (nodes, paths) = rotated.contraRotate().unzip
        nodes.zip(paths.rotate(-1).map(_.reverse))
      }
    }

    def fullNodeNeighbors: nPaths = {
      val neighb: nodesL     = node.neighbors.toList //; logger.debug("\nNeighbor nodes found: " + neighb)
      val start: graph.NodeT = neighb.minBy(_.toOuter) //; logger.debug("\nStarting neighbor node chosen: " + first)
      val (nodes, paths): (nodesL, List[nodesL]) = findPathFull(
        neighb.filterNot(_ == start),
        List((start, List())),
        Set(node)
      ).unzip //; logger.debug("\nnodes: " + nodes); logger.debug("\npaths: " + paths)
      val first                = nodes.head
      val last                 = nodes.last
      val block                = paths.flatten.toSet ++ nodes.init.tail.flatMap(_.neighbors) - first - last
      val lastPath: nodesL     = first.shortestWithBlocksTo(last, block).get.nodes.toList
      val paths2: List[nodesL] = paths.filterNot(_.isEmpty) :+ lastPath //; logger.debug("\npaths2: " + paths2)
      reorderFull(nodes.zip(paths2.headLastConcat)).map({ case (n, path) ⇒ (n, path.tail) })
    }

    /**
      * if node is full, neighbors ordered from min with direction to lower adjacent
      * if node is not full, ordered from min endpoint to other endpoint
      *
      * @return list of ordered neighbors nodes and ordered path nodes of the underlying p-gon to reach the next one
      */
    def nodeNeighbors: nPaths = if (node.isPerimeter) node.perimeterNodeNeighbors else node.fullNodeNeighbors

    def outerNodeNeighbors: List[(Int, List[Int])] = {
      val (nodes, paths) = node.nodeNeighbors.unzip
      nodes.map(_.toOuter).zip(paths.map(_.map(_.toOuter)))
    }
  }

  implicit final class ExtEdge(edge: graph.EdgeT) {

    /**
      * edge part of the perimeter, with at least one end directing to only 2 other nodes
      * (this condition is NOT necessarily true for all the edges in the perimeter)
      *
      * @return
      */
    def isPerimeterLoose: Boolean = edge._1.isMinimal || edge._2.isMinimal

    def isPerimeterLooseProg(out: Int, same: Boolean = true): Boolean = {
      val (d1, d2) = (edge._1.degree, edge._2.degree)
      if (same) d1 == out && d2 == out
      else (d1 == out && d2 == out + 1) || (d1 == out + 1 && d2 == out)
    }

    def isPerimeter: Boolean = orderedEdges.contains(edge)

  }

  // ----------------- perimeter -------------------

  val perimeter: graph.Path = {

    /**
      * if no perimeter loose edges with outdegree 2 found, this method recursively scan for others
      *
      * @param out  outdegree of one endpoint
      * @param same if true other endpoint same outdegree, false +1
      * @return
      */
    def progLooseEdges(out: Int, same: Boolean): List[graph.EdgeT] =
      graph.edges.filter(_.isPerimeterLooseProg(out, same)).toList match {
        case Nil if same ⇒ progLooseEdges(out, same = false) // increase
        case Nil         ⇒ progLooseEdges(out + 1, same = true) // increase
        case es          ⇒ es
      }

    val periGraph: Graph[Int, UnDiEdge] = {

      // adding edges to perimeter until is completed
      def loop(p: Graph[Int, UnDiEdge]): Graph[Int, UnDiEdge] = {
        if (p.isConnected && p.isCyclic) p
        else {
          // find an edge with endpoint
          val e: p.EdgeT = p.edges.find((e: p.EdgeT) ⇒ e._1.degree == 1 || e._2.degree == 1).get
          // n1 node of the endpoint
          val (n1, n2): (graph.NodeT, graph.NodeT) =
            if (e._1.degree == 1) (graph get e._1, graph get e._2) else (graph get e._2, graph get e._1)
          // nodes potentially on the perimeter
          val candidates: List[graph.NodeT] = n1.diSuccessors.toList.filterNot(_ == n2)
          // next is the farthest from n2 not passing through n1
          val next: graph.NodeT = candidates.maxBy(_.shortestWithBlocksTo(n2, Set(n1)).get.size)
          loop(p ++ List(n1.toOuter ~ next.toOuter))
        }
      }

      // start with loose (easy to find) edges
      val es: List[UnDiEdge[Int]] = (graph.edges.filter(_.isPerimeterLoose).toList match {
        case Nil  ⇒ progLooseEdges(3, same = true) // if nothing progressively explore higher degrees
        case some ⇒ some
      }).map(_.toOuter)

      loop(Graph() ++ es)
    }

    require(periGraph.isCyclic, "perimeter not cyclic")

    require(periGraph.nodes.forall(_.degree == 2), "perimeter not simple: " + periGraph)

    val nodeOrdering = periGraph.NodeOrdering(Ordering.Int.compare(_, _))

    /**
      * ordered from lowest node in the direction where the lower neighbor is found
      */
    val orderedCycle: periGraph.Cycle = periGraph.nodes.minBy(_.toOuter).withOrdering(nodeOrdering).findCycle.get

    val ns = orderedCycle.nodes.toList
    ns.tail.foldLeft(graph.newPathBuilder(graph get ns.head.toOuter))((pb, n) ⇒ pb += (graph get n.toOuter)).result()
  }

  val orderedNodes: List[graph.NodeT] = perimeter.nodes.toList :+ perimeter.nodes.head

  val orderedEdges: List[graph.EdgeT] = perimeter.edges.toList :+ graph.get(
    perimeter.nodes.last.toOuter ~ perimeter.nodes.head.toOuter)

  val (neighsNodes, neighsPaths): (List[List[graph.NodeT]], List[List[List[graph.NodeT]]]) =
    orderedNodes.init.map(_.nodeNeighbors.unzip).unzip

  val vertexes: List[Vertex] = neighsPaths.map(paths ⇒ Vertex.p(paths.init.map(_.size + 2)))

  // implies satisfying requirements of UnitSimplePgon
  val polygon: UnitSimplePgon = new UnitSimplePgon(vertexes.map(v ⇒ new PointPolar(1.0, τ / 2 - v.α)))

  def isPolygonSymmetricTo(that: T): Boolean = this.polygon.lαs.isRotationOrReflectionOf(that.polygon.lαs)

  // ----------------- to cartesian coords -------------------

  def toTessellMap: TessellMap = {

    val size = graph.nodes.size //; logger.debug("\n\ntarget size: " + size)

    /**
      *  try to find one node not yet mapped with at least two neighbors nodes mapped
      */
    def findAddable(mapped: List[Int]): Option[Int] =
      graph.nodes.toList
        .map(_.toOuter)
        .diff(mapped)
        .find(n ⇒ (graph get n).neighbors.toList.map(_.toOuter).intersect(mapped).lengthCompare(2) >= 0)

    def loop(tm: TessellMap): TessellMap = {

      /**
        *  try to find one node already mapped with at least three nodes neighbors
        *  of which at least two already mapped and at least one node not yet mapped
        */
      def findCompletable(mapped: List[Int]): Option[Int] =
        mapped.find(node ⇒ {
          val neighbors = (graph get node).neighbors.toList.map(_.toOuter)
          val hasEnoughNeighborsMapped = neighbors.intersect(mapped) match {
            case Nil           ⇒ false
            case _ :: Nil      ⇒ false
            case f :: s :: Nil ⇒ !tm.hasOnSameLine(f, s)
            case _             ⇒ true
          }
          hasEnoughNeighborsMapped && neighbors.diff(mapped).nonEmpty
        })

      if (tm.m.size == size) tm
      else {
        val mapped: List[Int] = tm.m.keys.toList
        val nexttm: Try[TessellMap] = findCompletable(mapped) match {
          case Some(node) ⇒ tm.completeNode(node, (graph get node).outerNodeNeighbors)
          case None ⇒
            findAddable(mapped) match {
              case Some(node) ⇒ tm.addFromNeighbors(node, (graph get node).outerNodeNeighbors)
              case None       ⇒ tm.addFromPerimeter(orderedNodes.init.map(_.toOuter), polygon.lαs)
            }
        }
        loop(nexttm.get)
      }
    }

    val firstNode: graph.NodeT = graph.nodes.minBy(_.toOuter)
    loop(TessellMap.firstThree(firstNode.toOuter, firstNode.outerNodeNeighbors))
  }

  def labelize: (Int, Point2D) ⇒ Label2D = { case (node, point) ⇒ new Label2D(point.c, node.toString) }

  def toLabels2D(tm: TessellMap): List[Label2D] = tm.m.map({ case (node, point) ⇒ labelize(node, point) }).toList

  def toSegments2D(tm: TessellMap): List[Segment2D] = graph.edges.toList.map(
    e ⇒ Segment2D.fromPoint2Ds(tm.m(e._1.toOuter), tm.m(e._2.toOuter))
  )

  def perimeterCoords(tm: TessellMap): List[Point2D] = orderedNodes.init.map(n ⇒ tm.m(n.toOuter))

  def toPerimeterPolygon(tm: TessellMap): Polygon = new Polygon(perimeterCoords(tm).map(_.c))

  def toPerimeterLabels2D(tm: TessellMap): List[Label2D] =
    orderedNodes.init
      .zip(perimeterCoords(tm))
      .map({
        case (node, point) ⇒ labelize(node.toOuter, point)
      })

  def toPolygons(tm: TessellMap): List[Polygon] = {

    def removeOrphans(g: Graph[Int, UnDiEdge]): Graph[Int, UnDiEdge] =
      g.nodes.filter(_.degree <= 1) match {
        case none if none.isEmpty ⇒ g
        case orphans              ⇒ removeOrphans(g -- orphans)
      }

    def createPoly(start: Int, end1: Int, end2: Int): Polygon = {
      val s = tm.m(start)
      //logger.debug("s " + s.toString + " e1 " + tm.m(end1).toString + " e2 " + tm.m(end2).toString)
      Polygon
        .createRegularFrom(
          Segment2D.fromPoint2Ds(s, tm.m(end1)),
          Segment2D.fromPoint2Ds(s, tm.m(end2))
        )
        .get
    }

    def loop(g: Graph[Int, UnDiEdge], ps: List[Polygon]): List[Polygon] = {
      //logger.debug("starting graph " + g.toString)
      if (g.isEmpty) ps
      else {
        val t = new TessellGraph(g)

        def isWorkable(n: t.graph.NodeT, degree: Int): Boolean = {

          def isOnPerimeter: Boolean = degree == 2 || t.orderedNodes.contains(n)

          def safeRemoval: Boolean = {
            val newg = removeOrphans(t.graph - n)
            newg.isEmpty || Try(new TessellGraph(newg)).isSuccess
          }

          n.degree == degree && isOnPerimeter && safeRemoval
        }

        def getPeriNeighbors(no: Int): List[Int] = {
          val all = t.orderedNodes.tail.map(_.toOuter)
          val i   = all.indexOf(no)
          val s   = all.size
          if (i == 0) List(all(1), all(s - 1))
          else List(all(i - 1), all((i + 1) % s))
        }

        t.graph.nodes.find(isWorkable(_, 2)) match {
          case Some(n) ⇒
            val no = n.toOuter
            //logger.debug("found node " + no.toString)
            //val neighbors = t.perimeter.get(no).neighbors.toList.map(_.toOuter)
            val neighbors = getPeriNeighbors(no)
            //logger.debug("neighbors node " + neighbors.toString)
            val p = createPoly(no, neighbors.head, neighbors(1))
            loop(removeOrphans(t.graph - no), ps :+ p)
          case None ⇒
            //logger.debug("found no workable 2-degree nodes")
            t.graph.nodes.find(isWorkable(_, 3)) match {
              case None ⇒ throw new NoSuchElementException("found no 3-degree nodes")
              case Some(n) ⇒
                val no = n.toOuter
                //logger.debug("found node " + no.toString)
                val neighbors = t.graph.get(no).neighbors.toList.map(_.toOuter)
                neighbors.diff(getPeriNeighbors(no)).headOption match {
                  case None ⇒ throw new NoSuchElementException("found no internal node")
                  case Some(internal) ⇒
                    val two_ps = for (external ← neighbors.diff(List(internal)))
                      yield createPoly(no, internal, external)
                    //logger.debug("polygons " + two_ps.toString + "\n")
                    loop(removeOrphans(t.graph - no), ps ++ two_ps)
                }
            }
        }
      }
    }

    loop(graph, Nil).distinctBy(_.barycenter == _.barycenter)
  }

  // ----------------- other stuff -------------------

  def pgonsMap: Map[Int, Int] = toPolygons(toTessellMap).groupBy(_.cs.size).map({ case (k, ps) ⇒ (k, ps.size) })

}

object TessellGraph extends Net {

  type Tessell = TessellGraph

  /**
    * create new starting with 1 p-gon
    *
    * @param n p-gon number of sides
    * @return
    */
  def poly(sides: Int): Tessell = new Tessell(Graph.from(Nil, for (i ← 1 to sides) yield i ~ (i % sides + 1)))

}
