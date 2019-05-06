package vision.id.tessella

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

import vision.id.tessella.Tessella.Tiling
import vision.id.tessella.Cartesian2D._
import vision.id.tessella.Polar.{PointPolar, UnitSimplePgon}
import vision.id.tessella.Tau.TAU

trait TilingUtils
    extends Border
    with Neighbors
    with OptionUtils
    with GraphUtils
    with MathUtils
    with TryUtils
    with DistinctUtils[Polygon]
    with Symmetry {

  final implicit class UTiling(tiling: Tiling) {

    private implicit class UNode(node: tiling.NodeT) {

      def isPerimeter: Boolean = perimeterOrderedNodes.contains(node.toOuter)

      def perimeterIndex: Int = perimeterOrderedNodes.indexOf(node.toOuter)

      def neighs: (List[Int], List[List[Int]]) = tiling.outerNodeHood(node.toOuter, perimeterOrderedNodes)

      def neighsSmart: (List[Int], List[List[Int]]) = perimeterIndex match {
        case -1 => neighs
        case i  => perimeterHoods(i)
      }

    }

    private def nodeOrdering = tiling.NodeOrdering(Ordering.Int.compare(_, _))

    private def perimeterCycle: tiling.Cycle = {
      val lowestPerimeterNode = tiling.edges.filter(_.isPerimeter.contains(true)).flatMap(_.toList).min(nodeOrdering)
      lowestPerimeterNode
        .withOrdering(nodeOrdering)
        .withSubgraph(edges = _.isPerimeter.contains(true))
        .findCycle
        .safeGet()
    }

    lazy val perimeterOrderedNodes: List[Int] = perimeterCycle.nodes.toList.map(_.toOuter)

    lazy val perimeterOrderedEdges: List[Side[Int]] = perimeterCycle.edges.map(_.toOuter).toList

    def perimeterEdgesByMin: List[Side[Int]] =
      perimeterOrderedEdges.sortWith((a, b) => {
        val nodesA = a.toList
        val nodesB = b.toList
        if (nodesA.min == nodesB.min) nodesA.sum < nodesB.sum else nodesA.min < nodesB.min
      })

    def perimeterMinEdge: Side[Int] = perimeterEdgesByMin.safeHead

    lazy val perimeterHoods: List[(List[Int], List[List[Int]])] =
      perimeterOrderedNodes.init.map(tiling.outerNodeHood(_, perimeterOrderedNodes))

    private def pathToVertex(path: List[List[Int]]): Vertex = Vertex.p(path.init.map(_.size + 2))

    lazy val vertexes: List[Vertex] = perimeterHoods.unzip match { case (_, paths) => paths.map(pathToVertex) }

    // implies satisfying requirements of UnitSimplePgon
    lazy val toPerimeterSimplePolygon: Try[UnitSimplePgon] =
      Try(new UnitSimplePgon(vertexes.map(v => new PointPolar(1.0, TAU / 2 - v.alpha))))

    def polygon: UnitSimplePgon = toPerimeterSimplePolygon.safeGet

    def isPolygonSymmetricTo(that: Tiling): Boolean = this.polygon.points.isRotationOrReflectionOf(that.polygon.points)

    def perimeterAngles: List[Double] = polygon.points.map(_.phi - TAU / 2)

    def toNodesMap: NodesMap = {

      val size = tiling.nodes.size

      @tailrec
      def loop(nm: NodesMap): NodesMap = {

        if (nm.m.size == size) nm
        else {
          val mapped: List[Int] = nm.m.keys.toList
          val newNm: Try[NodesMap] = tiling.findCompletable(mapped, nm) match {
            case Some(node) => nm.completeNode(node, (tiling get node).neighsSmart)
            case None =>
              tiling.findAddable(mapped) match {
                case Some(node) => nm.addFromNeighbors(node, (tiling get node).neighsSmart)
                case None       => nm.addFromPerimeter(perimeterOrderedNodes.init, toPerimeterSimplePolygon.safeGet.points)
              }
          }
          loop(newNm.safeGet)
        }
      }

      val firstNode: tiling.NodeT = tiling.nodes.minBy(_.toOuter)
      loop(NodesMap.firstThree(firstNode.toOuter, firstNode.neighsSmart))
    }

    def hasGap: Boolean = Try(toNodesMap) match {
      case Success(nm) =>
        tiling.edges.exists(
          _.nodes.map(_.toOuter).toList.onlyTwoElements((f, s) => !nm.m(f).isUnitDistance(nm.m(s))))
      case Failure(_) => true
    }

    // ----------------- to cartesian coords -------------------

    def labelize: (Int, Point2D) => Label2D = { case (node, point) => new Label2D(point, node.toString) }

    def toLabels2D(nm: NodesMap): List[Label2D] = nm.m.map({ case (node, point) => labelize(node, point) }).toList

    def perimeterCoords(nm: NodesMap): List[Point2D] = perimeterOrderedNodes.init.map(nm.m(_))

    def toPerimeterPolygon(nm: NodesMap): Polygon = new Polygon(perimeterCoords(nm))

    def toPerimeterLabels2D(nm: NodesMap): List[Label2D] =
      perimeterOrderedNodes.init
        .zip(perimeterCoords(nm))
        .map({
          case (node, point) => labelize(node, point)
        })

    def toPolygons(nm: NodesMap): List[Polygon] = {

      // clone mutable tiling
      val t = tiling.clone()

      @tailrec
      def loop(ps: List[Polygon]): List[Polygon] =
        if (t.isEmpty) ps
        else {

          def isOnPerimeter(n: t.NodeT): Boolean = n.degree == 2 || n.outgoing.exists(_.isPerimeter.contains(true))

          val tPeriNodes = t.nodes.filter(isOnPerimeter)

          def onPeri(neighbors: Set[t.NodeT]): Set[t.NodeT] = neighbors.intersect(tPeriNodes)

          def subtractableNodes(n: t.NodeT, neighbors: Set[t.NodeT]): Set[t.NodeT] =
            onPeri(neighbors).toList.foldLeft(Set(): Set[t.NodeT])((s, adj) =>
              n.withSubgraph(edges = _.isPerimeter.contains(true), nodes = _ != adj) pathUntil (_.degree > 2) match {
                case None       => s ++ Set(n)
                case Some(path) => s ++ path.nodes.init
            })

          def isTriangle(n: t.NodeT): Int =
            n.neighbors.toList.onlyTwoElements((f, s) => if (f.neighbors.contains(s)) 1 else 0)

          def getSubtraction(degree: Int): Option[(t.NodeT, Set[t.NodeT])] =
            if (degree == 2 && tPeriNodes.forall(_.degree == 2)) {
              val n     = tPeriNodes.toList.safeHead
              val neigh = n.neighbors
              t --= tPeriNodes
              Some(n, neigh)
            } else {
              val filtered =
                t.nodes
                  .filter(n => n.degree == degree && (if (degree == 3) tPeriNodes.contains(n) else true))
              val sorted = if (degree == 2) filtered.toList.sortWith(isTriangle(_) > isTriangle(_)) else filtered
              sorted.foreach(n => {
                val hood                   = n.neighbors
                val subtract: Set[t.NodeT] = if (degree == 3) Set(n) else subtractableNodes(n, hood)
                Try(t --= subtract) match {
                  case Success(_) => return Some(n, hood)
                  case Failure(_) => if (!t.hasPerimeterSet) t.setPerimeter
                }
              })
              None
            }

          getSubtraction(2) match {
            case Some((n, neighbors)) =>
              onPeri(neighbors).toList.map(_.toOuter) match {
                case f :: s :: Nil => loop(ps :+ nm.createPoly(n.toOuter, f, s))
                case _             => throw new NoSuchElementException("perimeter neighbors must be two")
              }
            case None =>
              getSubtraction(3) match {
                case None => throw new NoSuchElementException("found no 3-degree nodes")
                case Some((n, neighbors)) =>
                  neighbors.diff(onPeri(neighbors)).headOption match {
                    case None => throw new NoSuchElementException("found no internal node")
                    case Some(internal) =>
                      val twoPs = for (external <- neighbors - internal)
                        yield nm.createPoly(n.toOuter, internal.toOuter, external.toOuter)
                      loop(ps ++ twoPs)
                  }
              }
          }
        }

      loop(Nil).distinctBy(_.barycenter == _.barycenter)
    }

    // ----------------- other stuff -------------------

    def pgonsMap: Map[Int, Int] = toPolygons(toNodesMap).groupBy(_.points.size).map({ case (k, ps) => (k, ps.size) })

    def toG: Graph[Int, UnDiEdge] = graphFrom(tiling)

  }

  private def graphFrom(tiling: Tiling): Graph[Int, UnDiEdge] =
    Graph.from(Nil, tiling.edges.toOuter.map(_.toList.onlyTwoElements(_ ~ _)))

}
