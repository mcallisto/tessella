package vision.id.tessella

import scala.util.{Failure, Success, Try}

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

import vision.id.tessella.Alias.Tiling
import vision.id.tessella.Cartesian2D.{Label2D, Point2D, Polygon, Segment2D}
import vision.id.tessella.Polar.{PointPolar, RegularPgon, UnitSimplePgon}
import vision.id.tessella.Tau.TAU

trait TilingUtils
    extends Border
    with Neighbors
    with OptionUtils
    with GraphUtils
    with TryUtils
    with AddUtils
    with MathUtils
    with DistinctUtils[Polygon]
    with Symmetry {

  final implicit class UTiling(tiling: Tiling) {

    private implicit class UNode(node: tiling.NodeT) {

      def isPerimeter: Boolean = perimeterOrderedNodes.contains(node.toOuter)

      def neighs: List[(Int, List[Int])] = tiling.outerNodeHood(node.toOuter, perimeterOrderedNodes)

      /**
        * get nodes ordered from a given start
        *
        * @param direction true is following orderedNodes, false other way
        * @throws NoSuchElementException if node is not found
        * @return
        */
      def startNodes(direction: Boolean = true): List[Int] = orderedIndex match {
        case i => perimeterOrderedNodes.init.rotateWithDirection(-i, direction)
      }

      private def orderedIndex: Int = perimeterOrderedNodes.indexOf(node.toOuter)

      def startVertexes(dir: Boolean = true): List[Vertex] = vertexes.rotateWithDirection(-orderedIndex, dir)

      /**
        * get the node where to attach a p-gon, it can move because of adjacent p-gons
        *
        * @param a   internal angle of the p-gon to be attached
        * @param dir direction along the perimeter, true is following perimeterOrderedNodes, false other way
        * @return id and number of edges to be subtracted from addition
        */
      def getAttachNodeOld(a: Double, dir: Boolean): Try[(Int, Int)] = Try {
        val ns = node.startNodes(dir)
        val vs = node.startVertexes(dir)
        vs.indices.foldLeft(0)((add, i) =>
          vs(i).alpha match {
            case adjacent if a + adjacent ~= TAU =>
              println(i)
              println(ns(i + 1))
              add + 1 // go to next
            case toomuch if a + toomuch > TAU => throw new IllegalArgumentException("angle more than full, dir: " + dir)
            case _                            => return Try(ns(i), add)
        })
        throw new IllegalArgumentException("no more sides")
      }

      def getAttachNode(a: Double, dir: Boolean): Try[(Int, List[Side[Int]])] = Try {
        val ns = node.startNodes(dir)
        val vs = node.startVertexes(dir)
        vs.indices.foldLeft(Nil: List[Side[Int]])((add, i) =>
          vs(i).alpha match {
            case adjacent if a + adjacent ~= TAU => add :+ Side(ns(i), ns(i + 1)) // go to next
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
      def orderedEndPoints: (tiling.NodeT, tiling.NodeT) = perimeterOrderedEdges.indexOf(edge.toOuter) match {
        case -1 => throw new IllegalArgumentException("cannot find edge")
        case i  => (tiling get perimeterOrderedNodes(i), tiling get perimeterOrderedNodes(i + 1))
      }

      def additionalEdges(edgesNumber: Int): Try[(List[Side[Int]], List[Side[Int]])] = {
        val (f, s) = edge.orderedEndPoints
        val angle  = RegularPgon.angleFrom(edgesNumber)
        f.getAttachNode(angle, dir = false)
          .flatMap({
            case (back_node, backOldPerimeterEdges) =>
              s.getAttachNode(angle, dir = true)
                .flatMap({
                  case (forward_node, forwardOldPerimeterEdges) =>
                    val ids: List[Int] =
                      getFreeIds(edgesNumber - 2 - backOldPerimeterEdges.size - forwardOldPerimeterEdges.size,
                                 graphFrom(tiling).emptiesMax)
                    val newPerimeterEdges: List[Side[Int]] = ids match {
                      case Nil => List(Side(back_node, forward_node))
                      case _ =>
                        ids.indices.tail.toList.map(i => Side(ids(i - 1), ids(i))) ++
                          List(Side(back_node, ids.safeHead), Side(ids.safeLast, forward_node))
                    }
                    Success((newPerimeterEdges, backOldPerimeterEdges ++ forwardOldPerimeterEdges))
                })
          })
      }

      /**
        * try to add a p-gon to the given edge
        *
        * @param edgesNumber sides of the reg p-gon to be added
        * @return
        */
      def addPgonOfEdges(edgesNumber: Int): Try[Tiling] =
        edge.additionalEdges(edgesNumber).flatMap({ case (newPerimeterEdges, _) => Try(tiling ++ newPerimeterEdges) })

    }

    def toG: Graph[Int, UnDiEdge] = graphFrom(tiling)

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

    def perimeterHoods: (List[List[Int]], List[List[List[Int]]]) =
      perimeterOrderedNodes.init.map(tiling.outerNodeHood(_, perimeterOrderedNodes).unzip).unzip

    def vertexes: List[Vertex] =
      perimeterHoods match { case (_, paths) => paths.map(path => Vertex.p(path.init.map(_.size + 2))) }

    // implies satisfying requirements of UnitSimplePgon
    def toPerimeterSimplePolygon: Try[UnitSimplePgon] =
      Try(new UnitSimplePgon(vertexes.map(v => new PointPolar(1.0, TAU / 2 - v.alpha))))

    def pippo: String = perimeterCycle.toString

    def toNodesMap: NodesMap = {

      val size = tiling.nodes.size

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

      def loop(g: Graph[Int, UnDiEdge], ps: List[Polygon]): List[Polygon] = {
        if (g.isEmpty) ps
        else {
          val t = Tiling.fromG(g)

          def getPeriNeighbors(no: Int): List[Int] = {
            val all = t.perimeterOrderedNodes.tail
            val i   = all.indexOf(no)
            val s   = all.size
            if (i == 0) List(all(1), all(s - 1))
            else List(all(i - 1), all((i + 1) % s))
          }

          def isWorkable(n: t.NodeT, degree: Int): Boolean = {

            def isOnPerimeter: Boolean = degree == 2 || t.perimeterOrderedNodes.contains(n.toOuter)

            def safeRemoval: Boolean = {
              val newg = (t.toG - n.toOuter).withoutOrphans
              newg.isEmpty || Try(Tiling.fromG(newg)).isSuccess
            }

            n.degree == degree && isOnPerimeter && safeRemoval
          }

          t.nodes.find(isWorkable(_, 2)) match {
            case Some(n) =>
              val no        = n.toOuter
              val neighbors = getPeriNeighbors(no)
              val p         = nm.createPoly(no, neighbors.head, neighbors(1))
              loop((t.toG - no).withoutOrphans, ps :+ p)
            case None =>
              t.nodes.find(isWorkable(_, 3)) match {
                case None => throw new NoSuchElementException("found no 3-degree nodes")
                case Some(n) =>
                  val no        = n.toOuter
                  val neighbors = t.get(no).neighbors.toList.map(_.toOuter)
                  neighbors.diff(getPeriNeighbors(no)).headOption match {
                    case None => throw new NoSuchElementException("found no internal node")
                    case Some(internal) =>
                      val twoPs = for (external <- neighbors.diff(List(internal)))
                        yield nm.createPoly(no, internal, external)
                      loop((t.toG - no).withoutOrphans, ps ++ twoPs)
                  }
              }
          }
        }
      }

      loop(graphFrom(tiling), Nil).distinctBy(_.barycenter == _.barycenter)
    }

    // ----------------- addition -------------------

    def addToEdgePgon(edge: Side[Int], edgesNumber: Int): Try[Tiling] = (tiling get edge).addPgonOfEdges(edgesNumber)

    // ----------------- other stuff -------------------

    def pgonsMap: Map[Int, Int] = toPolygons(toNodesMap).groupBy(_.cs.size).map({ case (k, ps) => (k, ps.size) })

  }

  private def graphFrom(tiling: Tiling): Graph[Int, UnDiEdge] =
    Graph.from(Nil, tiling.edges.toOuter.map(_.toList match {
      case vertex1 :: vertex2 :: Nil => vertex1 ~ vertex2
      case _                         => throw new Error
    }))

}
