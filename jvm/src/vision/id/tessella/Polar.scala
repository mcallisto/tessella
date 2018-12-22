package vision.id.tessella

import java.lang.Math.{cos, sin, tan}
import scala.util.Try

import com.typesafe.scalalogging.Logger

import vision.id.tessella.Tau.TAU
import vision.id.tessella.Cartesian2D._

object Polar {

  /**
    * polar coordinates
    *
    * @param c side (radius or length) and angle in radians, from origin
    */
  class PointPolar(val c: Coords2D) extends CoordsOps /*with Ordered[PointPolar]*/ {

    /**
      * normalized angle, always 0 ≤ phi ≤ TAU
      */
    val (r, phi): (Double, Double) = c match { case (radius, angle) => (radius, mod(angle, TAU)) }

    override def hashCode: Int = 41 * (41 + r.hashCode()) + phi.hashCode

    override def equals(other: Any): Boolean = other match {
      case that: PointPolar => (that canEqual this) && equal(this.toCartesianCoords2D, that.toCartesianCoords2D)
      case _                => false
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[PointPolar]

    def toCartesianCoords2D: Coords2D = (r * cos(phi), r * sin(phi))

    def toPoint2D: Point2D = new Point2D(toCartesianCoords2D)

    def toSegment2D: Segment2D = new Segment2D(orig, toCartesianCoords2D)

    def sum(c1: Coords2D): PointPolar = new PointPolar(op(c, c1, _ + _))

    def sum(that: PointPolar): PointPolar = sum(that.c)

    def rotate(a: Double): PointPolar = sum(0, a)

    def reflect: PointPolar = new PointPolar(r, TAU - phi)

    override def toString: String = r + "~>" + phi.toRoundedDegrees() + "°"

  }

  /**
    * polygonal chain
    *
    * @param pps list of polar points (sides and angles)
    */
  class Polyline(val pps: List[PointPolar]) extends CoordsOps {

    val logger = Logger("POLYLINE")

    def ls: List[Double] = pps.map(_.r)

    /**
      * lines of length 0 are not allowed
      */
    require(!ls.contains(0.0))

    def phis: List[Double] = pps.map(_.phi)

    def n: Int = pps.size

    def length: Double = ls.foldLeft(0.0)(_ + _)

    def isClose: Boolean = end == Point2D.origin

    override def toString: String = pps.map(_.toString).mkString(",")

    def equalCoords(that: Polyline): Boolean = this.pps.zip(that.pps).forall({ case (p1, p2) => p1 == p2 })

    private def follow: ((Point2D, Double), PointPolar) => (Point2D, Double) =
      (acc, lphi) => acc match { case (point, alpha) => (point.sum(lphi.rotate(alpha)), alpha + lphi.phi) }

    def end: Point2D = pps.foldLeft((Point2D.origin, 0.0))(follow) match { case (p, _) => p }

    /**
      * reflected polar points
      *
      * @return
      */
    def reflectedPoints: List[PointPolar] = pps.map(_.reflect)

    /**
      * reflection
      *
      * @return
      */
    def reflect: Polyline = new Polyline(reflectedPoints)

    /**
      * list of points
      *
      * @param a starting rotation angle
      * @return
      */
    def toPoint2Ds(a: Double = 0.0): List[Point2D] = pps.scanLeft((Point2D.origin, a))(follow).map({ case (p, _) => p })

    /**
      * @param a starting rotation angle
      * @return
      */
    def toPolyline2D(a: Double = 0.0): Polyline2D = new Polyline2D(toPoint2Ds(a).map(_.c))

    /**
      * if there are consecutive lines with a flat angle, merge them
      *
      * @return
      */
    def simplify: Polyline =
      if (n < 2) this
      else {
        // add ending line, so we do not finish with a straight
        val las = pps :+ new PointPolar(0.0, 1.0)
        val (newPps, _) = las.foldLeft((List(): List[PointPolar], 0.0))((l, lphi) =>
          l match {
            case (acc, tot) if (lphi.phi % TAU) ~= 0.0 => (acc, tot + lphi.r) // found a straight line
            case (acc, tot) if tot > 0.0 => (acc ++ List(new PointPolar(tot, 0.0), lphi), 0.0) // previous straight
            case (acc, _)                => (acc :+ lphi, 0.0) // not straight
        })
        // remove ending line
        new Polyline(newPps.init)
      }

    /**
      * list of segments
      *
      * @param a starting rotation angle
      * @return
      */
    def toSegments2D(a: Double = 0.0): List[Segment2D] = {
      val cs = toPoint2Ds(a).map(_.c)
      (for (i <- 1 to n) yield new Segment2D(cs(i - 1), cs(i))).toList
    }

    /**
      * check all segments one against the other
      * find at least one intersection
      *
      * @return
      */
    def isSelfIntersecting: Boolean = {
      val ss = simplify.toSegments2D()
      val s  = ss.size
      for {
        i <- 0 until s
        j <- i until s
      } if (i != j && ss(i).isNotJoiningButIntersecting(ss(j))) return true
      false
    }

    /**
      * find all intersections
      *
      * @return
      */
    def selfIntersections: List[(Segment2D, Segment2D)] = {
      //logger.debug("self")
      val ss = simplify.toSegments2D()
      val s  = ss.size
      (for {
        i <- 0 until s
        j <- i until s
        if i != j && ss(i).isNotJoiningButIntersecting(ss(j))
      } yield (ss(i), ss(j))).toList
    }

  }

  class UnitPolyline(pps: List[PointPolar]) extends Polyline(pps) {

    require(ls == List.fill(n)(1.0))

    override def length: Double = n

  }

  object UnitPolyline {

    def ofAngles(alphas: List[Double]): UnitPolyline = new UnitPolyline(alphas.map(new PointPolar(1.0, _)))

  }

  class Pgon(pps: List[PointPolar]) extends Polyline(pps) {

    require(n > 2, this)

    require(isClose, "pgon not close, it should end at origin, not at: " + end)

    /**
      *
      * @param a starting rotation angle
      * @return
      */
    override def toPoint2Ds(a: Double = 0.0): List[Point2D] = super.toPoint2Ds(a).init

    /**
      * list of segments
      *
      * @param a starting rotation angle
      * @return
      */
    override def toSegments2D(a: Double = 0.0): List[Segment2D] = {
      val cs = super.toPoint2Ds(a).map(_.c)
      (for (i <- 1 to n) yield new Segment2D(cs(i - 1), cs(i))).toList
    }

    def toPolygon(a: Double = 0.0): Polygon = new Polygon(toPoint2Ds(a).map(_.c))

    /**
      * check all points (vertices) one against the other
      * find at least two with same coords
      *
      * @return
      */
    def isTouching: Boolean = {
      val ps = toPoint2Ds().map(_.c)
      val s  = ps.size
      for {
        i <- 0 until s
        j <- i until s
      } if (i != j && equal(ps(i), ps(j))) return true
      false
    }

    def touches: List[Coords2D] = {
      val ps = toPoint2Ds().map(_.c)
      val s  = ps.size
      (for {
        i <- 0 until s
        j <- i until s
        if i != j && equal(ps(i), ps(j))
      } yield ps(i)).toList
    }

    def perimeter: Double = length

  }

  class SimplePgon(pps: List[PointPolar]) extends Pgon(pps) {

    /**
      * cannot have line going backwards on top of the previous one
      *
      * @todo check if commented out version is better
      */
    require(!phis.contains(TAU / 2), phis)
//    require(!phis.exists(~=(_, TAU / 2)), phis)

    require(!isTouching, "touches: " + touches)

    require(!isSelfIntersecting, "intersections: " + selfIntersections)

  }

  class UnitSimplePgon(pps: List[PointPolar]) extends SimplePgon(pps) {

    require(ls == List.fill(n)(1.0))

    override def perimeter: Double = n

  }

  object UnitSimplePgon {

    def ofAngles(alphas: List[Double]): UnitSimplePgon = new UnitSimplePgon(alphas.map(new PointPolar(1.0, _)))

  }

  class RegularPgon(pps: List[PointPolar]) extends SimplePgon(pps) with ListUtils {

    require(ls.distinct.lengthCompare(1) == 0)

    require(phis.distinct.lengthCompare(1) == 0)

    /**
      * polar angle
      *
      * @return
      */
    val phi: Double = phis.safeHead

    val l: Double = ls.safeHead

    override def toString: String = "{" + n.toString + "}"

    /**
      * interior angle
      *
      * @return radians
      */
    def alpha: Double = TAU / 2 - phi

    /**
      * exterior angle
      *
      * @return radians
      */
    def beta: Double = TAU - alpha

    /**
      * inradius
      * radius of a polygon's incircle
      * in a regular polygon, segment from center to mid of the side, that is apothem
      *
      * @return
      */
    def r: Double = (1.0 / tan(TAU / 2 / n)) * l / 2

    /**
      * circumradius
      * radius of the circle inside which the polygon can be inscribed
      * in a regular polygon, segment from center to angle
      *
      * @return
      */
    def R: Double = (1.0 / sin(TAU / 2 / n)) * l / 2

    /**
      * area
      *
      * @return square units
      */
    def area: Double = length * r / 2

  }

  object RegularPgon extends MathUtils {

    val logger = Logger("REGULAR PGON")

    def angle(n: Int): Double = ((n - 2) * TAU / 2) / n

    def polarAngle(n: Int): Double = TAU / 2 - angle(n)

    /**
      * inverse of above method angle
      */
    def sides(alpha: Double): Try[Int] = Try {
      val n       = 2 / (1 - (alpha / (TAU / 2))) //; logger.debug(n.toString)
      val rounded = n.roundAt(0) //; logger.debug(rounded.toString)
      if ((n - rounded) ~= (0, stdPrecision * 10))
        rounded.toInt
      else
        throw new IllegalArgumentException("interior angle  not compatible with regular polygon")
    }

    def ofSides(n: Int, l: Double): RegularPgon = new RegularPgon(List.fill(n)(new PointPolar(l, polarAngle(n))))

  }

  class UnitRegularPgon(pps: List[PointPolar]) extends RegularPgon(pps) {

    require(ls.safeHead == 1.0)

    override def perimeter: Double = n

  }

  object UnitRegularPgon {

    def ofSides(n: Int): UnitRegularPgon =
      new UnitRegularPgon(List.fill(n)(new PointPolar(1.0, RegularPgon.polarAngle(n))))

  }

}
