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
    extends Perimeter
    with Neighbors
    with DistinctUtils[Polygon] {

  protected type T = TessellGraph

  val logger = Logger("TESSELL")

  require(graph.isConnected, "tessell not connected")

  require(graph.nodes.forall(_.toOuter > 0), "nodes with non positive ids: " + graph.nodes.filterNot(_.toOuter > 0))

  require(graph.nodes.forall(_.isRegular), "nodes with wrong number of edges: " + graph.nodes.filterNot(_.isRegular))

  implicit final class ExtNode(node: graph.NodeT) {

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

    // used for now in tests only
    def getNeighsPaths: List[(Int, List[Int])] = graph.outerNodeNeighbors(graph get node, periNodes)

  }

  // ----------------- perimeter -------------------

  val (periNodes, periEdges): (List[Int], List[UnDiEdge[Int]]) = graph.perimeterNodesEdges

  val (periNeighs, periPaths): (List[List[Int]], List[List[List[Int]]]) =
    periNodes.init.map(graph.outerNodeNeighbors(_, periNodes).unzip).unzip

  val vertexes: List[Vertex] = periPaths.map(paths ⇒ Vertex.p(paths.init.map(_.size + 2)))

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
          case Some(node) ⇒ tm.completeNode(node, graph.outerNodeNeighbors(node, periNodes))
          case None ⇒
            findAddable(mapped) match {
              case Some(node) ⇒ tm.addFromNeighbors(node, graph.outerNodeNeighbors(node, periNodes))
              case None       ⇒ tm.addFromPerimeter(periNodes.init, polygon.lαs)
            }
        }
        loop(nexttm.get)
      }
    }

    val firstNode: graph.NodeT = graph.nodes.minBy(_.toOuter)
    loop(TessellMap.firstThree(firstNode.toOuter, graph.outerNodeNeighbors(firstNode, periNodes)))
  }

  def labelize: (Int, Point2D) ⇒ Label2D = { case (node, point) ⇒ new Label2D(point.c, node.toString) }

  def toLabels2D(tm: TessellMap): List[Label2D] = tm.m.map({ case (node, point) ⇒ labelize(node, point) }).toList

  def toSegments2D(tm: TessellMap): List[Segment2D] = graph.edges.toList.map(
    e ⇒ Segment2D.fromPoint2Ds(tm.m(e._1.toOuter), tm.m(e._2.toOuter))
  )

  def perimeterCoords(tm: TessellMap): List[Point2D] = periNodes.init.map(tm.m(_))

  def toPerimeterPolygon(tm: TessellMap): Polygon = new Polygon(perimeterCoords(tm).map(_.c))

  def toPerimeterLabels2D(tm: TessellMap): List[Label2D] =
    periNodes.init
      .zip(perimeterCoords(tm))
      .map({
        case (node, point) ⇒ labelize(node, point)
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

          def isOnPerimeter: Boolean = degree == 2 || t.periNodes.contains(n.toOuter)

          def safeRemoval: Boolean = {
            val newg = removeOrphans(t.graph - n)
            newg.isEmpty || Try(new TessellGraph(newg)).isSuccess
          }

          n.degree == degree && isOnPerimeter && safeRemoval
        }

        def getPeriNeighbors(no: Int): List[Int] = {
          val all = t.periNodes.tail
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
    * @param sides p-gon number of sides
    * @return
    */
  def poly(sides: Int): Tessell = new Tessell(Graph.from(edges = for (i ← 1 to sides) yield i ~ (i % sides + 1)))

}
