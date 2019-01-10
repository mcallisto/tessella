package vision.id.tessella

import scala.util.{Success, Try}

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

import vision.id.tessella.Alias.Tiling
import vision.id.tessella.Cartesian2D.{Label2D, Point2D, Polygon, Segment2D}
import vision.id.tessella.Polar.{PointPolar, RegularPgon, UnitSimplePgon}
import vision.id.tessella.Tau.TAU

trait Methods
    extends Perimeter
    with Neighbors
    with DistinctUtils[Polygon]
    with TryUtils
    with GraphUtils
    with MathUtils
    with AddUtils {

  final implicit class XTiling(tiling: Tiling) {

    private implicit class XNode(node: tiling.NodeT) {

      def isPerimeter: Boolean = periNodes.contains(node.toOuter)

      def neighs: List[(Int, List[Int])] = tiling.outerNodeHood(node.toOuter, periNodes)

      /**
        * get nodes ordered from a given start
        *
        * @param direction true is following orderedNodes, false other way
        * @throws NoSuchElementException if node is not found
        * @return
        */
      def startNodes(direction: Boolean = true): List[Int] = orderedIndex match {
        case i => periNodes.init.rotateWithDirection(-i, direction)
      }

      private def orderedIndex: Int = periNodes.indexOf(node.toOuter)

      def startVertexes(dir: Boolean = true): List[Vertex] = vertexes.rotateWithDirection(-orderedIndex, dir)

      /**
        * get the node where to attach a p-gon, it can move because of adjacent p-gons
        *
        * @param a   internal angle of the p-gon to be attached
        * @param dir direction along the perimeter, true is following perimeterOrderedNodes, false other way
        * @return id and number of edges to be subtracted from addition
        */
      def getAttachNode(a: Double, dir: Boolean): Try[(Int, Int)] = Try {
        val ns = node.startNodes(dir)
        val vs = node.startVertexes(dir)
        vs.indices.foldLeft(0)((add, i) =>
          vs(i).alpha match {
            case adjacent if a + adjacent ~= TAU => add + 1 // go to next
            case toomuch if a + toomuch > TAU    => throw new IllegalArgumentException("angle more than full, dir: " + dir)
            case _                               => return Try(ns(i), add)
        })
        throw new IllegalArgumentException("no more sides")
      }

    }

    private implicit class XEdge(edge: tiling.EdgeT) {

      /**
        * @throws NoSuchElementException if edge is not found
        * @return (first, second) endpoint nodes of the edge ordered
        */
      def orderedEndPoints: (tiling.NodeT, tiling.NodeT) = periEdges.indexOf(edge.toOuter) match {
        case -1 => throw new IllegalArgumentException("cannot find edge")
        case i  => (tiling get periNodes(i), tiling get periNodes(i + 1))
      }

      def additionalEdges(sides: Int): Try[List[UnDiEdge[Int]]] = {
        val (f, s) = edge.orderedEndPoints
        val angle  = RegularPgon.angle(sides)
        f.getAttachNode(angle, dir = false)
          .flatMap({
            case (back_node, back_excess) =>
              s.getAttachNode(angle, dir = true)
                .flatMap({
                  case (forward_node, forward_excess) =>
                    val ids: List[Int] = getFreeIds(sides - 2 - back_excess - forward_excess, tiling.emptiesMax)
                    val es: List[UnDiEdge[Int]] = ids match {
                      case Nil => List(back_node ~ forward_node)
                      case _ =>
                        ids.indices.tail.toList.map(i => ids(i - 1) ~ ids(i)) ++
                          List(back_node ~ ids.safeHead, ids.safeLast ~ forward_node)
                    }
                    Success(es)
                })
          })
      }

      /**
        * try to add a p-gon to the given edge
        *
        * @param sides sides of the reg p-gon to be added
        * @return
        */
      def addPgonOfSides(sides: Int): Try[Tiling] = edge.additionalEdges(sides).flatMap(es => Try(tiling ++ es))

    }

    // ----------------- perimeter -------------------

    val (periNodes, periEdges): (List[Int], List[UnDiEdge[Int]]) = tiling.perimeterNodesEdges

    val (periNeighs, periPaths): (List[List[Int]], List[List[List[Int]]]) =
      periNodes.init.map(node => (tiling get node).neighs.unzip).unzip

    val vertexes: List[Vertex] = periPaths.map(paths => Vertex.p(paths.init.map(_.size + 2)))

    // implies satisfying requirements of UnitSimplePgon
    val polygon: UnitSimplePgon = new UnitSimplePgon(vertexes.map(v => new PointPolar(1.0, TAU / 2 - v.alpha)))

    def isPolygonSymmetricTo(that: Tiling): Boolean = this.polygon.pps.isRotationOrReflectionOf(that.polygon.pps)

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

    def toNodesMap: NodesMap = {

      val size = tiling.nodes.size

      def loop(tm: NodesMap): NodesMap = {

        if (tm.m.size == size) tm
        else {
          val mapped: List[Int] = tm.m.keys.toList
          val nexttm: Try[NodesMap] = tiling.findCompletable(mapped, tm) match {
            case Some(node) => tm.completeNode(node, (tiling get node).neighs)
            case None =>
              tiling.findAddable(mapped) match {
                case Some(node) => tm.addFromNeighbors(node, (tiling get node).neighs)
                case None       => tm.addFromPerimeter(periNodes.init, polygon.pps)
              }
          }
          loop(nexttm.get)
        }
      }

      val firstNode: tiling.NodeT = tiling.nodes.minBy(_.toOuter)
      loop(NodesMap.firstThree(firstNode.toOuter, firstNode.neighs))
    }

    def labelize: (Int, Point2D) => Label2D = { case (node, point) => new Label2D(point.c, node.toString) }

    def toLabels2D(tm: NodesMap): List[Label2D] = tm.m.map({ case (node, point) => labelize(node, point) }).toList

    def toGonals(tm: NodesMap): List[List[Point2D]] =
      mapGonals.map({ case (_, nodes) => nodes.map(tm.m(_)) }).toList

    def toSegments2D(tm: NodesMap): List[Segment2D] =
      tiling.edges.toList
        .map(_.toOuter)
        .map(
          e => Segment2D.fromPoint2Ds(tm.m(e._n(0)), tm.m(e._n(1)))
        )

    def perimeterCoords(tm: NodesMap): List[Point2D] = periNodes.init.map(tm.m(_))

    def toPerimeterPolygon(tm: NodesMap): Polygon = new Polygon(perimeterCoords(tm).map(_.c))

    def toPerimeterLabels2D(tm: NodesMap): List[Label2D] =
      periNodes.init
        .zip(perimeterCoords(tm))
        .map({
          case (node, point) => labelize(node, point)
        })

    def toPolygons(tm: NodesMap): List[Polygon] = {

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

      loop(tiling, Nil).distinctBy(_.barycenter == _.barycenter)
    }

    // ----------------- addition -------------------

    def addToEdgePgon(edge: UnDiEdge[Int], sides: Int): Try[Tiling] = (tiling get edge).addPgonOfSides(sides)

    // ----------------- other stuff -------------------

    def pgonsMap: Map[Int, Int] = toPolygons(toNodesMap).groupBy(_.cs.size).map({ case (k, ps) => (k, ps.size) })

    def toG: Graph[Int, UnDiEdge] = Graph.from(Nil, tiling.edges)

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
