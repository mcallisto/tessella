package vision.id.tessella

import scala.util.Try

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge

import vision.id.tessella.Alias.Tiling
import vision.id.tessella.Cartesian2D.{Label2D, Point2D, Polygon, Segment2D}
import vision.id.tessella.Polar.{PointPolar, UnitSimplePgon}
import vision.id.tessella.Tau.TAU

trait Methods extends Perimeter with Neighbors with DistinctUtils[Polygon] with TryUtils with GraphUtils {

  final implicit class XTiling(graph: Tiling) {

    private implicit class XNode(node: graph.NodeT) {

      def isPerimeter: Boolean = periNodes.contains(node.toOuter)

      def neighs: List[(Int, List[Int])] = graph.outerNodeHood(node.toOuter, periNodes)
    }

    // ----------------- perimeter -------------------

    val (periNodes, periEdges): (List[Int], List[UnDiEdge[Int]]) = graph.perimeterNodesEdges

    val (periNeighs, periPaths): (List[List[Int]], List[List[List[Int]]]) =
      periNodes.init.map(node => (graph get node).neighs.unzip).unzip

    val vertexes: List[Vertex] = periPaths.map(paths => Vertex.p(paths.init.map(_.size + 2)))

    // implies satisfying requirements of UnitSimplePgon
    val polygon: UnitSimplePgon = new UnitSimplePgon(vertexes.map(v => new PointPolar(1.0, TAU / 2 - v.alpha)))

    def isPolygonSymmetricTo(that: Tiling): Boolean = this.polygon.pps.isRotationOrReflectionOf(that.polygon.pps)

    // ----------------- gonality -------------------

    /**
      * @return map of different type of vertices and nodes where they are found
      */
    def mapGonals: Map[Full, List[Int]] =
      graph.nodes
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

    def toTessellMap: TessellMap = {

      val size = graph.nodes.size

      def loop(tm: TessellMap): TessellMap = {

        if (tm.m.size == size) tm
        else {
          val mapped: List[Int] = tm.m.keys.toList
          val nexttm: Try[TessellMap] = graph.findCompletable(mapped, tm) match {
            case Some(node) => tm.completeNode(node, (graph get node).neighs)
            case None =>
              graph.findAddable(mapped) match {
                case Some(node) => tm.addFromNeighbors(node, (graph get node).neighs)
                case None       => tm.addFromPerimeter(periNodes.init, polygon.pps)
              }
          }
          loop(nexttm.get)
        }
      }

      val firstNode: graph.NodeT = graph.nodes.minBy(_.toOuter)
      loop(TessellMap.firstThree(firstNode.toOuter, firstNode.neighs))
    }

    def labelize: (Int, Point2D) => Label2D = { case (node, point) => new Label2D(point.c, node.toString) }

    def toLabels2D(tm: TessellMap): List[Label2D] = tm.m.map({ case (node, point) => labelize(node, point) }).toList

    def toGonals(tm: TessellMap): List[List[Point2D]] =
      mapGonals.map({ case (_, nodes) => nodes.map(tm.m(_)) }).toList

    def toSegments2D(tm: TessellMap): List[Segment2D] =
      graph.edges.toList
        .map(_.toOuter)
        .map(
          e => Segment2D.fromPoint2Ds(tm.m(e._n(0)), tm.m(e._n(1)))
        )

    def perimeterCoords(tm: TessellMap): List[Point2D] = periNodes.init.map(tm.m(_))

    def toPerimeterPolygon(tm: TessellMap): Polygon = new Polygon(perimeterCoords(tm).map(_.c))

    def toPerimeterLabels2D(tm: TessellMap): List[Label2D] =
      periNodes.init
        .zip(perimeterCoords(tm))
        .map({
          case (node, point) => labelize(node, point)
        })

    def toPolygons(tm: TessellMap): List[Polygon] = {

      def createPoly(start: Int, end1: Int, end2: Int): Polygon = {
        val s = tm.m(start)
        Polygon
          .createRegularFrom(
            Segment2D.fromPoint2Ds(s, tm.m(end1)),
            Segment2D.fromPoint2Ds(s, tm.m(end2))
          )
          .safeGet
      }

      def loop(g: Graph[Int, UnDiEdge], ps: List[Polygon]): List[Polygon] = {
        if (g.isEmpty) ps
        else {
          val t = Tiling.fromG(g)
          t.nodes.find(isWorkable(t)(_, 2)) match {
            case Some(n) =>
              val no        = n.toOuter
              val neighbors = getPeriNeighbors(t, no)
              val p         = createPoly(no, neighbors.head, neighbors(1))
              loop((t.toG - no).withoutOrphans, ps :+ p)
            case None =>
              t.nodes.find(isWorkable(t)(_, 3)) match {
                case None => throw new NoSuchElementException("found no 3-degree nodes")
                case Some(n) =>
                  val no        = n.toOuter
                  val neighbors = t.get(no).neighbors.toList.map(_.toOuter)
                  neighbors.diff(getPeriNeighbors(t, no)).headOption match {
                    case None => throw new NoSuchElementException("found no internal node")
                    case Some(internal) =>
                      val twoPs = for (external <- neighbors.diff(List(internal)))
                        yield createPoly(no, internal, external)
                      loop((t.toG - no).withoutOrphans, ps ++ twoPs)
                  }
              }
          }
        }
      }

      loop(graph, Nil).distinctBy(_.barycenter == _.barycenter)
    }

    // ----------------- other stuff -------------------

    def pgonsMap: Map[Int, Int] = toPolygons(toTessellMap).groupBy(_.cs.size).map({ case (k, ps) => (k, ps.size) })

    def toG: Graph[Int, UnDiEdge] = Graph.from(Nil, graph.edges)

  }

  private def isWorkable(graph: Tiling)(n: graph.NodeT, degree: Int): Boolean = {

    def isOnPerimeter: Boolean = degree == 2 || graph.periNodes.contains(n.toOuter)

    def safeRemoval: Boolean = {
      val newg = (graph.toG - n.toOuter).withoutOrphans
      newg.isEmpty || Try(Tiling.fromG(newg)).isSuccess
    }

    n.degree == degree && isOnPerimeter && safeRemoval
  }

  private def getPeriNeighbors(graph: Tiling, no: Int): List[Int] = {
    val all = graph.periNodes.tail
    val i   = all.indexOf(no)
    val s   = all.size
    if (i == 0) List(all(1), all(s - 1))
    else List(all(i - 1), all((i + 1) % s))
  }

}
