package vision.id.tessella

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

import vision.id.tessella.Tessella.Tiling
import vision.id.tessella.Cartesian2D.{Label2D, Point2D, Polygon, Segment2D}
import vision.id.tessella.Polar.{PointPolar, UnitSimplePgon}
import vision.id.tessella.Tau.TAU

trait TilingUtils
    extends Border
    with Neighbors
    with OptionUtils
    with GraphUtils
    with TryUtils
    with DistinctUtils[Polygon]
    with Symmetry {

  final implicit class UTiling(tiling: Tiling) {

    private implicit class UNode(node: tiling.NodeT) {

      def isPerimeter: Boolean = perimeterOrderedNodes.contains(node.toOuter)

      def neighs: List[(Int, List[Int])] = tiling.outerNodeHood(node.toOuter, perimeterOrderedNodes)

    }

    private def nodeOrdering = tiling.NodeOrdering(Ordering.Int.compare(_, _))

    private def perimeterCycle: tiling.Cycle = {
      val lowestPerimeterNode = tiling.edges.filter(_.isPerimeter.contains(true)).flatMap(_.toList).min(nodeOrdering)
      (lowestPerimeterNode
        .withOrdering(nodeOrdering)
        .withSubgraph(edges = _.isPerimeter.contains(true)) findCycle)
        .safeGet()
    }

    lazy val perimeterOrderedNodes: List[Int] = perimeterCycle.nodes.map(_.toOuter).toList

    lazy val perimeterOrderedEdges: List[Side[Int]] = perimeterCycle.edges.map(_.toOuter).toList

    lazy val perimeterHoods: (List[List[Int]], List[List[List[Int]]]) =
      perimeterOrderedNodes.init.map(tiling.outerNodeHood(_, perimeterOrderedNodes).unzip).unzip

    lazy val vertexes: List[Vertex] =
      perimeterHoods match { case (_, paths) => paths.map(path => Vertex.p(path.init.map(_.size + 2))) }

    // implies satisfying requirements of UnitSimplePgon
    lazy val toPerimeterSimplePolygon: Try[UnitSimplePgon] =
      Try(new UnitSimplePgon(vertexes.map(v => new PointPolar(1.0, TAU / 2 - v.alpha))))

    def toNodesMap: NodesMap = {

      val size = tiling.nodes.size

      @tailrec
      def loop(nm: NodesMap): NodesMap = {

        if (nm.m.size == size) nm
        else {
          val mapped: List[Int] = nm.m.keys.toList
          val nexttm: Try[NodesMap] = tiling.findCompletable(mapped, nm) match {
            case Some(node) => nm.completeNode(node, (tiling get node).neighs)
            case None =>
              tiling.findAddable(mapped) match {
                case Some(node) => nm.addFromNeighbors(node, (tiling get node).neighs)
                case None       => nm.addFromPerimeter(perimeterOrderedNodes.init, toPerimeterSimplePolygon.safeGet.pps)
              }
          }
          loop(nexttm.get)
        }
      }

      val firstNode: tiling.NodeT = tiling.nodes.minBy(_.toOuter)
      loop(NodesMap.firstThree(firstNode.toOuter, firstNode.neighs))
    }

    def hasGap: Boolean = Try(toNodesMap) match {
      case Success(nm) =>
        tiling.edges.exists(_.nodes.map(_.toOuter) match {
          case n1 :: n2 :: Nil => !nm.m(n1).isUnitDistance(nm.m(n2))
          case _               => throw new Error
        })
      case Failure(_) => true
    }

    // ----------------- gonality -------------------

    /**
      * @return map of different type of vertices and nodes where they are found
      */
    def mapGonals: Map[Full, List[Int]] =
      tiling.nodes
        .filterNot(_.isPerimeter)
        .toList
        .map(n =>
          n.neighs.unzip match {
            case (_, paths) => (Full.p(paths.map(_.size + 2)).minor, n)
        })
        .groupBy({ case (vertices, _) => vertices })
        .map({ case (vertices, sidesNodes) => vertices -> sidesNodes.map({ case (_, node) => node.toOuter }) })

    /**
      * @return number of different type of vertices
      */
    def gonality: Int = mapGonals.size

    // ----------------- to cartesian coords -------------------

    def labelize: (Int, Point2D) => Label2D = { case (node, point) => new Label2D(point.c, node.toString) }

    def toLabels2D(nm: NodesMap): List[Label2D] = nm.m.map({ case (node, point) => labelize(node, point) }).toList

    def toGonals(nm: NodesMap): List[List[Point2D]] =
      mapGonals.map({ case (_, nodes) => nodes.map(nm.m(_)) }).toList

    def toSegments2D(nm: NodesMap): List[Segment2D] =
      tiling.edges.toList.map(_.nodes.map(_.toOuter) match {
        case n1 :: n2 :: Nil => Segment2D.fromPoint2Ds(nm.m(n1), nm.m(n2))
        case _               => throw new Error
      })

    def perimeterCoords(nm: NodesMap): List[Point2D] = perimeterOrderedNodes.init.map(nm.m(_))

    def toPerimeterPolygon(nm: NodesMap): Polygon = new Polygon(perimeterCoords(nm).map(_.c))

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

          def getSubtraction(degree: Int): Option[(t.NodeT, Set[t.NodeT])] =
            if (degree == 2 && tPeriNodes.forall(_.degree == 2)) {
              val n     = tPeriNodes.toList.safeHead
              val neigh = n.neighbors
              t --= tPeriNodes
              Some(n, neigh)
            } else {
              t.nodes
                .filter(n => n.degree == degree && (if (degree == 3) tPeriNodes.contains(n) else true))
                .foreach(n => {
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

    def pgonsMap: Map[Int, Int] = toPolygons(toNodesMap).groupBy(_.cs.size).map({ case (k, ps) => (k, ps.size) })

    def toG: Graph[Int, UnDiEdge] = graphFrom(tiling)

  }

  private def graphFrom(tiling: Tiling): Graph[Int, UnDiEdge] =
    Graph.from(Nil, tiling.edges.toOuter.map(_.toList match {
      case vertex1 :: vertex2 :: Nil => vertex1 ~ vertex2
      case _                         => throw new Error
    }))

}
