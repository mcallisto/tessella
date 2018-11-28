package vision.id.tessella

import java.lang.Math.{cos, sin, tan}
import scala.util.Try

import com.typesafe.scalalogging.Logger

import vision.id.tessella.Tau.τ
import vision.id.tessella.Cartesian2D._

object Polar {

  /**
    * polar coordinates
    *
    * @param c side (radius or length) and angle in radians, from origin
    */
  class PointPolar(val c: Coords2D) extends CoordsOps /*with Ordered[PointPolar]*/ {

    /**
      * normalized angle, always 0 ≤ ϕ ≤ τ
      */
    val (r, ϕ): (Double, Double) = c match { case (radius, angle) ⇒ (radius, mod(angle, τ)) }

    override def hashCode: Int = 41 * (41 + r.hashCode()) + ϕ.hashCode

    override def equals(other: Any): Boolean = other match {
      case that: PointPolar ⇒ (that canEqual this) && equal(this.toCartesianCoords2D, that.toCartesianCoords2D)
      case _                ⇒ false
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[PointPolar]

    def toCartesianCoords2D: Coords2D = (r * cos(ϕ), r * sin(ϕ))

    def toPoint2D: Point2D = new Point2D(toCartesianCoords2D)

    def toSegment2D: Segment2D = new Segment2D(orig, toCartesianCoords2D)

    def sum(c1: Coords2D): PointPolar = new PointPolar(op(c, c1, _ + _))

    def sum(that: PointPolar): PointPolar = sum(that.c)

    def rotate(a: Double): PointPolar = sum(0, a)

    def reflect: PointPolar = new PointPolar(r, τ - ϕ)

    override def toString: String = r + "⇝" + ϕ.toRoundedDegrees() + "°"

  }

  /**
    * polygonal chain
    *
    * @param lϕs list of polar points (sides and angles)
    */
  class Polyline(val lϕs: List[PointPolar]) extends CoordsOps {

    val logger = Logger("POLYLINE")

    def ls: List[Double] = lϕs.map(_.r)

    /**
      * lines of length 0 are not allowed
      */
    require(!ls.contains(0.0))

    def ϕs: List[Double] = lϕs.map(_.ϕ)

    def n: Int = lϕs.size

    def length: Double = ls.foldLeft(0.0)(_ + _)

    def isClose: Boolean = end == Point2D.origin

    override def toString: String = lϕs.map(_.toString).mkString(",")

    def equalCoords(that: Polyline): Boolean = this.lϕs.zip(that.lϕs).forall({ case (p1, p2) ⇒ p1 == p2 })

    private def follow: ((Point2D, Double), PointPolar) ⇒ (Point2D, Double) =
      (acc, lϕ) ⇒ acc match { case (point, α) ⇒ (point.sum(lϕ.rotate(α)), α + lϕ.ϕ) }

    def end: Point2D = lϕs.foldLeft((Point2D.origin, 0.0))(follow)._1

    /**
      * reflected polar points
      *
      * @return
      */
    def reflectedPoints: List[PointPolar] = lϕs.map(_.reflect)

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
    def toPoint2Ds(a: Double = 0.0): List[Point2D] = lϕs.scanLeft((Point2D.origin, a))(follow).map(_._1)

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
        val las = lϕs :+ new PointPolar(0.0, 1.0)
        val r = las.foldLeft((List(): List[PointPolar], 0.0))((l, lϕ) ⇒
          l match {
            case (acc, tot) ⇒
              // found a straight line
              if ((lϕ.ϕ % τ) ≈ 0.0) (acc, tot + lϕ.r)
              // not straight, but there was a straight before
              else if (tot > 0.0) (acc ++ List(new PointPolar(tot, 0.0), lϕ), 0.0)
              // not straight
              else (acc :+ lϕ, 0.0)
        })
        // remove ending line
        new Polyline(r._1.init)
      }

    /**
      * list of segments
      *
      * @param a starting rotation angle
      * @return
      */
    def toSegments2D(a: Double = 0.0): List[Segment2D] = {
      val cs = toPoint2Ds(a).map(_.c)
      (for (i ← 1 to n) yield new Segment2D(cs(i - 1), cs(i))).toList
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
        i ← 0 until s
        j ← i until s
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
        i ← 0 until s
        j ← i until s
        if i != j && ss(i).isNotJoiningButIntersecting(ss(j))
      } yield (ss(i), ss(j))).toList
    }

  }

  class UnitPolyline(val lαs: List[PointPolar]) extends Polyline(lαs) {

    require(ls == List.fill(n)(1.0))

    override def length: Double = n

  }

  object UnitPolyline {

    def ofAngles(αs: List[Double]): UnitPolyline = new UnitPolyline(αs.map(new PointPolar(1.0, _)))

  }

  class Pgon(val lαs: List[PointPolar]) extends Polyline(lαs) {

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
      (for (i ← 1 to n) yield new Segment2D(cs(i - 1), cs(i))).toList
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
        i ← 0 until s
        j ← i until s
      } if (i != j && equal(ps(i), ps(j))) return true
      false
    }

    def touches: List[Coords2D] = {
      val ps = toPoint2Ds().map(_.c)
      val s  = ps.size
      (for {
        i ← 0 until s
        j ← i until s
        if i != j && equal(ps(i), ps(j))
      } yield ps(i)).toList
    }

    def perimeter: Double = length

  }

  class SimplePgon(lαs: List[PointPolar]) extends Pgon(lαs) {

    /**
      * cannot have line going backwards on top of the previous one
      *
      * @todo check if commented out version is better
      */
    require(!ϕs.contains(τ / 2), ϕs)
//    require(!ϕs.exists(~=(_, τ / 2)), ϕs)

    require(!isTouching, "touches: " + touches)

    require(!isSelfIntersecting, "intersections: " + selfIntersections)

  }

  class UnitSimplePgon(lαs: List[PointPolar]) extends SimplePgon(lαs) {

    require(ls == List.fill(n)(1.0))

    override def perimeter: Double = n

  }

  object UnitSimplePgon {

    def ofAngles(αs: List[Double]): UnitSimplePgon = new UnitSimplePgon(αs.map(new PointPolar(1.0, _)))

  }

  class RegularPgon(lϕs: List[PointPolar]) extends SimplePgon(lϕs) {

    require(ls.distinct.lengthCompare(1) == 0)

    require(ϕs.distinct.lengthCompare(1) == 0)

    /**
      * polar angle
      *
      * @return
      */
    val ϕ: Double = ϕs.head

    val l: Double = ls.head

    override def toString: String = "{" + n.toString + "}"

    /**
      * interior angle
      *
      * @return radians
      */
    def α: Double = τ / 2 - ϕ

    /**
      * exterior angle
      *
      * @return radians
      */
    def β: Double = τ - α

    /**
      * inradius
      * radius of a polygon's incircle
      * in a regular polygon, segment from center to mid of the side, that is apothem
      *
      * @return
      */
    def r: Double = (1.0 / tan(τ / 2 / n)) * l / 2

    /**
      * circumradius
      * radius of the circle inside which the polygon can be inscribed
      * in a regular polygon, segment from center to angle
      *
      * @return
      */
    def R: Double = (1.0 / sin(τ / 2 / n)) * l / 2

    /**
      * area
      *
      * @return square units
      */
    def area: Double = length * r / 2

  }

  object RegularPgon extends MathUtils {

    val logger = Logger("REGULAR PGON")

    def angle(n: Int): Double = ((n - 2) * τ / 2) / n

    def polarAngle(n: Int): Double = τ / 2 - angle(n)

    /**
      * inverse of above method angle
      */
    def sides(α: Double): Try[Int] = Try {
      val n       = 2 / (1 - (α / (τ / 2))) //; logger.debug(n.toString)
      val rounded = n.roundAt(0) //; logger.debug(rounded.toString)
      if ((n - rounded) ≈ (0, stdPrecision * 10))
        rounded.toInt
      else
        throw new IllegalArgumentException("interior angle  not compatible with regular polygon")
    }

    def ofSides(n: Int, l: Double): RegularPgon = new RegularPgon(List.fill(n)(new PointPolar(l, polarAngle(n))))

  }

  class UnitRegularPgon(lαs: List[PointPolar]) extends RegularPgon(lαs) {

    require(ls.head == 1.0)

    override def perimeter: Double = n

  }

  object UnitRegularPgon {

    def ofSides(n: Int): UnitRegularPgon =
      new UnitRegularPgon(List.fill(n)(new PointPolar(1.0, RegularPgon.polarAngle(n))))

  }

}
