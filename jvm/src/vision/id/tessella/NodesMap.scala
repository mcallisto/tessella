package vision.id.tessella

import scala.util.{Failure, Success, Try}

import com.typesafe.scalalogging.Logger

import vision.id.tessella.Cartesian2D.{Point2D, Points2D, Polygon, Segment2D}
import vision.id.tessella.Polar.{PointPolar, Polyline, UnitRegularPgon}
import vision.id.tessella.Tau.TAU

class NodesMap(val m: Map[Int, Point2D]) extends ListUtils with MathUtils with TryUtils {

  val logger = Logger("NODESMAP")

  def toPrintable: Map[Int, String] = m.map({ case (node, point) => (node, point.toString) })

  def addFromThree(node: Int, mapped: (Int, Int, Int)): Try[NodesMap] =
    Try(m.get(node) match {
      case Some(_) => throw new IllegalArgumentException("node already mapped")
      case None =>
        val (n1, n2, n3) = mapped
        val points       = List(n1, n2, n3).map(m)
        points.safeHead.getFourth(points(1), points(2)) match {
          case Success(point) => new NodesMap(m + (node -> point))
          case Failure(e)     => throw e
        }
    })

  /**
    * add nodes from neighbors of unmapped node
    *
    * @param node  unmapped node
    * @param neigh neighbors data
    * @return
    */
  def addFromNeighbors(node: Int, neigh: (List[Int], List[List[Int]])): Try[NodesMap] = m.get(node) match {
    case Some(_) =>
      Try(throw new IllegalArgumentException("node already mapped"))
    case None =>
      //logger.debug("\nnode: " + node)
      val (nodes, _) = neigh
      if (nodes.lengthCompare(2) < 0)
        Try(throw new IllegalArgumentException("not enough neighbors"))
      else {
        nodes.filter(m.get(_).isDefined) match {
          case n_start :: n_end :: Nil =>
            val point = m(n_start).unitContacts(m(n_end)).safeGet match {
              case p :: Nil => p //; logger.debug("\np found because unique: " + p); p
              case p1 :: p2 :: Nil =>
                val v = m.values.toList //; logger.debug("\nv: " + v)
                (v.contains(p1), v.contains(p2)) match {
                  case (true, true)  => throw new Error("both points already existing")
                  case (true, false) => p2 //; logger.debug("\np2 found by exclusion: " + p2); p2
                  case (false, true) => p1 //; logger.debug("\np1 found by exclusion: " + p1); p1
                  case _             => throw new IllegalArgumentException("cannot be found")
                }
              case _ => throw new Error
            }
            Try(new NodesMap(m + (node -> point)))
          case f :: s :: t :: _ => addFromThree(node, (f, s, t))
          case _                => Try(throw new IllegalArgumentException("not enough mapped neighbors"))
        }
      }
  }

  def hasOnSameLine(f: Int, s: Int): Boolean = Segment2D.fromPoint2Ds(m(f), m(s)).length ~= 2.0

  implicit final class OuterNode(node: Int) {

    def isOnSameLineOf(other: Int): Boolean = hasOnSameLine(node, other)

  }

  private def getNeighborsUnmapped(node: Int, nodes: List[Int], all: IndexedSeq[Double]): Map[Int, Point2D] =
    (for {
      i <- nodes.indices
      if m.get(nodes(i)).isEmpty
    } yield nodes(i) -> m(node).sum(new PointPolar(1.0, all(i)))).toMap

  /**
    * get angle and direction
    */
  private def findAngleDir(node: Int,
                           nodes: List[Int],
                           nStart: Int,
                           t: List[Int],
                           interiors: List[Double]): (Double, Int) = {
    // find first and second mapped neighbors nodes (not on the same line with node)
    val nEnd  = t.find(!_.isOnSameLineOf(nStart)).safeGet()
    val start = nodes.indexOf(nStart)
    val end   = nodes.indexOf(nEnd)
    // find angle between start and end based on coords
    val s1          = Segment2D.fromPoint2Ds(m(node), m(nStart)).angle
    val s2          = Segment2D.fromPoint2Ds(m(node), m(nEnd)).angle
    val angleCoords = mod(s2 - s1, TAU) //; logger.debug("\nangleCoords: " + angleCoords)
    // find angle between start and end based on ordered perimeter
    val angPerim = (start until end).foldLeft(0.0)((acc, i) => acc + interiors(i)) //; logger.debug("\nangPerim: " + angPerim)
    // find orientation
    val dir = if (angleCoords ~= angPerim) 1 else -1 //; logger.debug("\ndir: " + dir)
    // find initial angle based on coords
    val angStart = (0 until start).foldLeft(0.0)((acc, i) => acc + interiors(i))
    (s1 - (angStart * dir), dir)
  }

  /**
    * add nodes from neighbors of mapped node and related p-gons
    *
    * @param node  mapped node
    * @param neigh neighbors data
    * @return
    */
  def completeNode(node: Int, neigh: (List[Int], List[List[Int]])): Try[NodesMap] = Try(
    if (m.get(node).isEmpty)
      throw new IllegalArgumentException("node not mapped")
    else {

      val (nodes, paths) = neigh //; logger.debug("\n\npaths: " + paths)
      nodes.filter(m.get(_).isDefined) match {
        case Nil =>
          throw new IllegalArgumentException("No neighbors mapped")
        case _ :: Nil =>
          throw new IllegalArgumentException("Only one neighbor mapped")
        case f :: s :: Nil if f.isOnSameLineOf(s) =>
          throw new IllegalArgumentException("2 neighbors mapped but aligned with node")
        case nStart :: t =>
          def getPgonsUnmapped(all: IndexedSeq[Double], interiors: List[Double], dir: Int): Map[Int, Point2D] = {
            // find p-gons coords not mapped yet
            nodes.indices
              .flatMap(i =>
                paths(i) match {
                  case Nil => List()
                  case path =>
                    val start = m(node).sum(new PointPolar(1.0, all(i)))
                    val (_, mapped) = path.init.indices.foldLeft((start, List(): List[(Int, Point2D)]))({
                      case ((pos, acc), j) =>
                        val newpos = pos.sum(new PointPolar(1.0, all(i) + (j + 1) * (Math.PI - interiors(i)) * dir))
                        val node   = path(j)
                        (newpos, if (m.get(node).isEmpty) acc :+ ((node, newpos)) else acc)
                    })
                    mapped
              })
              .toMap
          }

          // map interior angles
          val interiors: List[Double] = paths.map({
            case Nil => 0
            case p   => UnitRegularPgon.ofEdges(p.size + 2).alpha
          })
          val (initialAngle, dir) = findAngleDir(node, nodes, nStart, t, interiors) //; logger.debug("\ninitialAngle: " + initialAngle)
          // find all neighbors angles
          val all               = paths.indices.scanLeft(initialAngle)((acc, i) => acc + interiors(i) * dir)
          val neighborsUnmapped = getNeighborsUnmapped(node, nodes, all) //; logger.debug("\nneighborsUnmapped: " + neighborsUnmapped)
          val pgonsUnmapped     = getPgonsUnmapped(all, interiors, dir) //; logger.debug("\npgonsUnmapped: " + pgonsUnmapped)
          new NodesMap(m ++ neighborsUnmapped ++ pgonsUnmapped)
      }

    }
  )

  /**
    * add nodes from the perimeter
    *
    * @param pon    perimeter ordered nodes
    * @param polars ordered polar coords of the perimeter
    * @return
    */
  def addFromPerimeter(pon: List[Int], polars: List[PointPolar]): Try[NodesMap] = Try {
    val mapped = m.keys.toList
    if (pon.diff(mapped).isEmpty)
      throw new IllegalArgumentException("found no extra nodes to map")
    else {
      //logger.debug("\npon: " + pon)
      // pairs of consecutive nodes
      val cs: List[(Int, Int)] = pon.circularSliding(2).toList.map(c => (c.safeHead, c(1)))
      cs.find({ case (first, second) => mapped.contains(first) && mapped.contains(second) }) match {
        case None                  => throw new IllegalArgumentException("no consecutive mapped nodes on perimeter")
        case Some((first, second)) => // found couple both mapped
          //logger.debug("\nc: " + (first, second))
          // shift everything to first node and then cut to second
          val i     = pon.indexOf(first)
          val poly  = new Polyline(polars.rotate(i).tail.init)
          val nodes = pon.rotate(i).tail.init
          // coords moved to second node and pointing in the right direction
          val angle        = Segment2D.fromPoint2Ds(m(first), m(second)).angle
          val points       = poly.toPoint2Ds(angle).map(_.sum(m(second))) //; logger.debug("\ncoords: " + points)
          val periUnmapped = nodes.zip(points).filter({ case (node, _) => !mapped.contains(node) }).toMap
          //logger.debug("\nadd: " + periUnmapped)
          new NodesMap(m ++ periUnmapped)
      }
    }
  }

  /**
    * give rotation dir in relation to two ordered nodes
    *
    * @param node1 first node
    * @param node2 second node
    * @return
    */
  def rotationDir(node1: Int, node2: Int): Boolean = {

    def getPoint(n: Int): Point2D = m.get(n) match {
      case Some(point) => point
      case None        => throw new IllegalArgumentException("node " + n + " does not belong to map")
    }

    val (point1, point2) = (getPoint(node1), getPoint(node2))

    def checkDirection(x: Double = 0.0): Boolean = {
      val vertex = Point2D.origin.move(x, 0.0)
      val angle1 = point1.angleFrom(vertex)
      val angle2 = point2.angleFrom(vertex)
      mod(angle1 - angle2, TAU) >= TAU / 2
    }

    if (point1.y ~= 0.0) {
      if (point2.y ~= 0.0) {
        point1.x >= point2.x
      } else checkDirection(1.0)
    } else checkDirection()

  }

  def diagonal: Double = {
    val (min, max) = new Points2D(m.values.toList.map(_.c)).getMinMax
    min.distanceFrom(max)
  }

  def createPoly(start: Int, end1: Int, end2: Int): Polygon = {
    val s = m(start)
    Polygon
      .createRegularFrom(
        Segment2D.fromPoint2Ds(s, m(end1)),
        Segment2D.fromPoint2Ds(s, m(end2))
      )
      .safeGet
  }

}

object NodesMap extends ListUtils {

  /**
    * map first three nodes
    *
    * @note in a tessellation grown from zero, this assure a stable drawing output
    * @param node  starting node, usually the lowest
    * @param neigh neighbors info
    * @return
    */
  def firstThree(node: Int, neigh: (List[Int], List[List[Int]])): NodesMap = {
    val (nodes, paths) = neigh
    val angle          = UnitRegularPgon.ofEdges(paths.safeHead.size + 2).alpha
    new NodesMap(
      Map(
        node           -> Point2D.origin,
        nodes.safeHead -> Point2D.origin.move(1.0, 0.0),
        nodes(1)       -> Point2D.origin.sum(new PointPolar(1.0, angle))
      ))
  }

}
