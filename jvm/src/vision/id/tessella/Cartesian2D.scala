package vision.id.tessella

import java.lang.Math.{atan2, hypot}

import scala.collection.SortedSet
import scala.util.{Failure, Success, Try}
import scala.xml._

import com.typesafe.scalalogging.Logger

import vision.id.tessella.Tau.TAU
import vision.id.tessella.Polar.{PointPolar, RegularPgon}

object Cartesian2D {

  type Coords2D = (Double, Double)

  trait CoordsOps extends MathUtils {

    val orig: Coords2D = (0.0, 0.0)

    /**
      * combine the xs and ys of two coords
      *
      * @param c1 coords 1
      * @param c2 coords 2
      * @param f  operation
      * @return
      */
    def op(c1: Coords2D, c2: Coords2D, f: (Double, Double) => Double): Coords2D = (c1, c2) match {
      case ((x1, y1), (x2, y2)) => (f(x1, x2), f(y1, y2))
    }

    def equal(c1: Coords2D, c2: Coords2D): Boolean = (c1, c2) match {
      case ((x1, y1), (x2, y2)) => (x1 ~= x2) && (y1 ~= y2)
    }

    def comparison(c1: Coords2D, c2: Coords2D): Int = (c1, c2) match {
      case ((x1, y1), (x2, y2)) =>
        if (x1 ~= x2) {
          if (y1 ~= y2) 0
          else y1.compare(y2)
        } else x1.compare(x2)
    }
  }

  class Point2D(val c: Coords2D) extends CoordsOps with Ordered[Point2D] {

    val logger = Logger("POINT2D")

    val (x, y): (Double, Double) = c

    override def hashCode: Int = 41 * (41 + x.hashCode()) + y.hashCode

    override def equals(other: Any): Boolean = other match {
      case that: Point2D => (that canEqual this) && equal(this.c, that.c)
      case _             => false
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[Point2D]

    override def toString: String = "{" + x.roundAt() + ":" + y.roundAt() + "}"

    def toPolar: PointPolar = new PointPolar(hypot(x, y), atan2(y, x))

    /**
      *
      * @param that other point
      * @return 0 if the same; negative if this < that; positive if this > that
      */
    def compare(that: Point2D): Int = comparison(this.c, that.c)

    def sum(c1: Coords2D): Point2D = new Point2D(op(c, c1, _ + _))

    def diff(c1: Coords2D): Point2D = new Point2D(op(c, c1, _ - _))

    def mult(c1: Coords2D): Point2D = new Point2D(op(c, c1, _ * _))

    def min(c1: Coords2D): Point2D = new Point2D(op(c, c1, Math.min))

    def max(c1: Coords2D): Point2D = new Point2D(op(c, c1, Math.max))

    def sum(that: Point2D): Point2D = sum(that.c)

    def diff(that: Point2D): Point2D = diff(that.c)

    def min(that: Point2D): Point2D = min(that.c)

    def max(that: Point2D): Point2D = max(that.c)

    def sum(polar: PointPolar): Point2D = sum(polar.toPoint2D)

    def move(x: Double, y: Double): Point2D = sum((x, y))

    def scale(x: Double, y: Double): Point2D = mult((x, y))

    def scale(xy: Double): Point2D = scale(xy, xy)

    def distanceFrom(that: Point2D): Double = this.diff(that).toPolar.r

    def angleFrom(that: Point2D): Double = this.diff(that).toPolar.phi

    def midCoords(that: Point2D, a: Double, d: Double): Coords2D = op(c, that.c, (is, at) => a * (at - is) / d)

    def midProportional(that: Point2D, a: Double, d: Double): Point2D = sum(midCoords(that, a, d))

    def halfWay(that: Point2D): Point2D = midProportional(that, 1.0, 2.0)

    /**
      * find the point of contacts between the unit circles with this and that centres
      *
      * @note https://stackoverflow.com/questions/3349125/circle-circle-intersection-points
      * @param that centre of other unit circle
      * @return
      */
    def unitContacts(that: Point2D): Try[List[Point2D]] =
      Try(distanceFrom(that) match {
        case zero if zero ~= 0.0 => throw new IllegalArgumentException("equal coords")
        case two if two ~= 2.0   => List(halfWay(that))
        case over if over > 2.0  => List()
        case d =>
          val a: Double   = (d * d) / (d * 2)
          val p2: Point2D = midProportional(that, a, d)
          val h: Double   = Math.sqrt(1.0 - a * a)
          val (px, py)    = midCoords(that, h, d)
          List(p2.sum(py, -px), p2.sum(-py, px))
      })

    def isUnitDistance(that: Point2D): Boolean = distanceFrom(that) ~= 1.0

    /**
      * get the point at unit distance from all three points, if exists
      *
      * @param second centre of unit circle
      * @param third  centre of unit circle
      * @return
      */
    def getFourth(second: Point2D, third: Point2D): Try[Point2D] =
      second.unitContacts(third) match {
        case Failure(e) => Failure(e)
        case Success(cs) =>
          Try(cs match {
            case Nil                                     => throw new IllegalArgumentException("out of reach")
            case coords :: Nil if isUnitDistance(coords) => coords
            case _ :: Nil                                => throw new IllegalArgumentException("one not matching")
            case c1 :: _ :: Nil if isUnitDistance(c1)    => c1
            case _ :: c2 :: Nil if isUnitDistance(c2)    => c2
            case _ :: _ :: Nil                           => throw new IllegalArgumentException("both not matching")
            case _                                       => throw new Error
          })
      }

    /**
      * flip horizontally along the vertical line with given x
      *
      * @param coordx vertical line coord
      * @return
      */
    def flipH(coordx: Double): Point2D = move(-2 * coordx, 0).scale(-1, 1)

    /**
      * rotate around centre of given angle
      *
      * @param angle  angle in radians
      * @param centre point
      * @return
      */
    def rotate(angle: Double, centre: Point2D = Point2D.origin): Point2D =
      centre.sum(centre.diff(this).toPolar.rotate(TAU / 2 + angle))

    /**
      * perp product
      * see http://geomalgorithms.com/vector_products.html#2D-Perp-Product
      *
      * @param that other point
      * @return
      */
    def perp(that: Point2D): Double = this.x * that.y - this.y * that.x

  }

  object Point2D extends CoordsOps {

    def origin: Point2D = new Point2D(orig)

  }

  class Label2D(val point: Point2D, s: String) extends CoordsOps with SVG {

    //override def toString: String = "(" + s + ")" + super.toString

    def toSVG(style: String = "fill:red"): Elem =
      addStyle(<text x={point.x.roundAt().toString} y={point.y.roundAt().toString}>{s}</text>, style)

    def sum(coords: Coords2D): Label2D = new Label2D(new Point2D(op(point.c, coords, _ + _)), s)

    def mult(coords: Coords2D): Label2D = new Label2D(new Point2D(op(point.c, coords, _ * _)), s)

    def sum(that: Point2D): Label2D = sum(that.c)

    def move(x: Double, y: Double): Label2D = sum(x, y)

    def scale(x: Double, y: Double): Label2D = mult(x, y)

    def scale(xy: Double): Label2D = scale(xy, xy)

  }

  class Segment2D(points: (Point2D, Point2D)) extends SVG with CoordsOps {

    val (s, e): (Point2D, Point2D) = points

    require(s != e, "endpoints must be different: " + s + " and " + e)

    override def hashCode: Int = 41 * (41 * (41 * (41 + s.x.hashCode()) + s.y.hashCode) + e.x.hashCode) + e.y.hashCode

    override def equals(other: Any): Boolean = other match {
      case that: Segment2D =>
        (that canEqual this) &&
          ((this.s == that.s && this.e == that.e) || (this.s == that.e && this.e == that.s))
      case _ => false
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[Segment2D]

    override def toString: String = "[" + s + "|" + e + "]"

    /**
      *
      * @param style style sheet
      * @return
      */
    def toSVG(style: String = "stroke:black"): Elem =
      addStyle(<line x1={s.x.roundAt().toString} y1={s.y.roundAt().toString}
                     x2={e.x.roundAt().toString} y2={e.y.roundAt().toString}/>,
               style)

    private def map2(coords: Coords2D, f: (Coords2D, Coords2D) => Coords2D): (Point2D, Point2D) =
      (new Point2D(f(s.c, coords)), new Point2D(f(e.c, coords)))

    def mapOp(coords: Coords2D, f: (Double, Double) => Double): (Point2D, Point2D) = map2(coords, op(_, _, f))

    def sum(coords: Coords2D): Segment2D = new Segment2D(mapOp(coords, _ + _))

    def mult(coords: Coords2D): Segment2D = new Segment2D(mapOp(coords, _ * _))

    def sum(point: Point2D): Segment2D = sum(point.c)

    def move(x: Double, y: Double): Segment2D = sum(x, y)

    def scale(x: Double, y: Double): Segment2D = mult(x, y)

    def scale(xy: Double): Segment2D = scale(xy, xy)

    def fromOrigin: Point2D = e.diff(s.c)

    def toPolar: PointPolar = fromOrigin.toPolar

    def length: Double = s.distanceFrom(e)

    def angle: Double = toPolar.phi

    def op2(f: (Point2D, Point2D) => Point2D): Point2D = f(s, e)

    def min: Point2D = op2(_.min(_))

    def max: Point2D = op2(_.max(_))

    def diff: Point2D = op2(_.diff(_))

    /**
      * @see http://geomalgorithms.com/a05-_intersect-1.html
      * @note touching of two endpoints is considered intersecting
      * @param that other segment
      * @return
      */
    def isIntersecting(that: Segment2D): Boolean = {

      val u: Point2D = this.e.diff(this.s)
      val v: Point2D = that.e.diff(that.s)
      val w: Point2D = this.s.diff(that.s)
      val d: Double  = u.perp(v)

      if (d ~= 0.0) { // parallel segments

        if (u.perp(w) != 0.0 || v.perp(w) != 0.0) false // but NOT collinear
        else { // collinear segments - get  overlap (or not)

          val w2: Point2D = this.e.diff(that.s)

          // endpoints of S1 in eqn for S2
          val (t0, t1): (Double, Double) = (v.x match {
            case 0.0 => (w.y / v.y, w2.y / v.y)
            case _   => (w.x / v.x, w2.x / v.x)
          }) match { // must have t0 smaller than t1
            case ok @ (a, b) if a < b => ok
            case ko                   => ko.swap
          }

          !(t0 > 1.0 || t1 < 0.0) // NO overlap
        }
      } else { // segments are skew and may intersect in a point

        v.perp(w) / d match { // intersect parameter for this segment
          case sI if sI < 0.0 || sI > 1.0 => false // no intersect with this
          case _ =>
            u.perp(w) / d match { // intersect parameter for that segment
              case tI if tI < 0.0 || tI > 1.0 => false // no intersect with that
              case _                          => true // intersect point (this.s + sI * u)
            }
        }

      }
    }

    /**
      * check if the given point is endpoint of the segment
      *
      * @param point point
      * @return
      */
    def isJoining(point: Point2D): Boolean = s == point || e == point

    /**
      * check if the given segment shares an endpoint with the segment
      *
      * @param that segment
      * @return
      */
    def isJoining(that: Segment2D): Boolean = this.isJoining(that.s) || this.isJoining(that.e)

    def isNotJoiningButIntersecting(that: Segment2D): Boolean = !this.isJoining(that) && this.isIntersecting(that)

    /**
      * test if a point is Left|On|Right of an infinite line
      *
      * @param point point
      * @return > 0 for p left of the line passing by segment, == 0 on the line, < 0 right of the line
      */
    def isLeft(point: Point2D): Double = (e.x - s.x) * (point.y - s.y) - (point.x - s.x) * (e.y - s.y)

    /**
      * winding number
      * see C++ code http://geomalgorithms.com/a03-_inclusion.html
      *
      * @param point to be checked for inclusion
      * @return
      */
    def windingNumber(point: Point2D): Int =
      s.y match {
        case startEqualLower if startEqualLower <= point.y =>
          if (e.y > point.y && isLeft(point) > 0)
            1 // an upward crossing and p left of edge
          else
            0
        case _ =>
          if (e.y <= point.y && isLeft(point) < 0)
            -1 // a downward crossing and p right of edge
          else
            0
      }

    def includesInBox(point: Point2D): Boolean =
      (point.x isInRange(s.x, e.x)) && (point.y isInRange(s.y, e.y))

    /**
      * flip horizontally along the vertical line with given x
      *
      * @param coordx vertical line coord
      * @return
      */
    def flipH(coordx: Double): Segment2D = new Segment2D(s.flipH(coordx), e.flipH(coordx))

    /**
      * rotate around centre of given angle
      *
      * @param angle  angle in radians
      * @param centre point
      * @return
      */
    def rotate(angle: Double, centre: Point2D = Point2D.origin): Segment2D =
      new Segment2D(s.rotate(angle, centre), e.rotate(angle, centre))

  }

  object Segment2D {

    def fromCoords2D(c1: Coords2D, c2: Coords2D): Segment2D = new Segment2D(new Point2D(c1), new Point2D(c2))

  }

  /**
    * ordered unit segment, first endpoint is required to be lower than second endpoint
    *
    * @param points endpoints
    */
  case class OrderedUnitSegment2D(points: (Point2D, Point2D))
      extends Segment2D(points)
      with Ordered[OrderedUnitSegment2D] {

    require(s < e, "endpoints must be ordered")
    require(length ~= 1.0, "length is not ~= 1.0")

    /**
      * @param that other segment
      * @return 0 if the same; negative if this < that; positive if this > that
      */
    def compare(that: OrderedUnitSegment2D): Int =
      if (this.s == that.s) {
        if (this.e == that.e) 0
        else this.e.compare(that.e)
      } else this.s.compare(that.s)

    override def sum(coords: Coords2D): OrderedUnitSegment2D = OrderedUnitSegment2D(mapOp(coords, _ + _))

    override def sum(point: Point2D): OrderedUnitSegment2D = sum(point.c)

    override def flipH(coordx: Double): OrderedUnitSegment2D =
      OrderedUnitSegment2D.fromPoint2Ds(s.flipH(coordx), e.flipH(coordx))

    override def rotate(angle: Double, centre: Point2D = Point2D.origin): OrderedUnitSegment2D =
      OrderedUnitSegment2D.fromPoint2Ds(s.rotate(angle, centre), e.rotate(angle, centre))

    /**
      * @param that other ordered segment
      * @return
      */
    def isIntersecting(that: OrderedUnitSegment2D): Boolean =
      if ((that.s.x - this.s.x) >> 1.0 || Math.abs(that.s.y - this.s.y) >> 2.0) false
      else super.isIntersecting(that)

    override def includesInBox(point: Point2D): Boolean =
      (point.x isInSortedRange(s.x, e.x)) && (point.y isInRange(s.y, e.y))

  }

  object OrderedUnitSegment2D {

    def fromSegment2D(segment: Segment2D): OrderedUnitSegment2D =
      OrderedUnitSegment2D.fromPoint2Ds(segment.s, segment.e)

    def fromPoint2Ds(point1: Point2D, point2: Point2D): OrderedUnitSegment2D =
      List(point1, point2).sorted.onlyTwoElements(OrderedUnitSegment2D(_, _))

  }

  class Polyline2D(val points: List[Point2D]) extends MathUtils with CoordsOps {

    def toPointsSVGAttribute: String =
      points.map(point => point.x.roundAt().toString + "," + point.y.roundAt().toString).mkString(" ")

    /**
      *
      * @param style style sheet
      * @return
      */
    def toSVG(style: String = "fill:none;stroke:black;stroke-width:3"): Elem =
      <polyline style={style} points={toPointsSVGAttribute}/>

    /**
      * map to coords function combining points
      *
      * @param coords coordinates
      * @param f      operation
      * @return
      */
    private def map2(coords: Coords2D, f: (Coords2D, Coords2D) => Coords2D): List[Point2D] =
      points.map(point => new Point2D(f(point.c, coords)))

    /**
      * map to coords function combining xs and ys
      *
      * @param coords coordinates
      * @param f      operation
      * @return
      */
    def mapOp(coords: Coords2D, f: (Double, Double) => Double): List[Point2D] = map2(coords, op(_, _, f))

    def sum(coords: Coords2D): Polygon = new Polygon(mapOp(coords, _ + _))

    def mult(coords: Coords2D): Polygon = new Polygon(mapOp(coords, _ * _))

    def sum(point: Point2D): Polygon = sum(point.c)

    def move(x: Double, y: Double): Polygon = sum(x, y)

    def scale(x: Double, y: Double): Polygon = mult(x, y)

    def scale(xy: Double): Polygon = scale(xy, xy)

    /**
      * get the lowest and higher points
      * delimiting the rectangular area covered by the points
      *
      * @return
      */
    def getMinMax: (Point2D, Point2D) = points.map(_.c) match {
      case Nil => (Point2D.origin, Point2D.origin)
      case coords =>
        val (cmin, cmax) = coords.foldLeft((orig, orig))({
          case ((min, max), c) => (op(min, c, Math.min), op(max, c, Math.max))
        })
        (new Point2D(cmin), new Point2D(cmax))
    }

  }

  class Polygon(points: List[Point2D]) extends Polyline2D(points) with ListUtils with TryUtils with Grid {

    /**
      *
      * @param style style sheet
      * @return
      */
    override def toSVG(style: String = "fill:none;stroke:green;stroke-width:3"): Elem =
      <polygon style={style} points={toPointsSVGAttribute}/>

    private def slide: List[List[Point2D]] = points.circularSliding(2).toList

    lazy val toSegments2D: List[Segment2D] =
      slide.map(_.onlyTwoElements(new Segment2D(_, _)))

    lazy val minX: Double = points.map(_.x).min

    lazy val maxX: Double = points.map(_.x).max

    // fails if not an unit pgon
    def toGrid: Try[Grid] =
      sequence(slide.map(_.onlyTwoElements((f, s) => Try(OrderedUnitSegment2D.fromPoint2Ds(f, s)))))
        .map(SortedSet(_: _*))

    def includes(point: Point2D): Boolean =
      toSegments2D.foldLeft(0)(_ + _.windingNumber(point)) != 0

    def includesEndpointsOf(segment: Segment2D): Boolean =
      includes(segment.s) && includes(segment.e)

    /**
      * @note valid for simple polygons only
      * @param segment segment to be checked for inclusion
      * @return
      */
    def includes(segment: Segment2D): Boolean =
      includesEndpointsOf(segment) && toSegments2D.forall(!segment.isIntersecting(_))

    def includesUnit(segment: OrderedUnitSegment2D): Boolean =
      (segment.s.x >~= minX) && (segment.e.x <~= maxX) && includes(new Segment2D(segment.s, segment.e))

    /**
      * @see https://gis.stackexchange.com/questions/22739/finding-center-of-geometry-of-object/22744#22744
      */
    def barycenter: Point2D = {
      val s      = points.size
      val (x, y) = points.map(_.c).foldLeft((0.0, 0.0))({ case ((sumX, sumY), (xx, yy)) => (sumX + xx, sumY + yy) })
      new Point2D(x / s, y / s)
    }

  }

  object Polygon extends MathUtils {

    val logger = Logger("POLYGON")

    def createRegularFrom(side: Segment2D, adjacent: Segment2D): Try[Polygon] = {
      require(side.s == adjacent.s)

      val l = side.length
      require(adjacent.length ~= l, l + " is different from " + adjacent.length)

      //logger.debug("side " + side)
      //logger.debug("adjacent " + adjacent)

      val sideAngle = side.toPolar.phi
      //logger.debug("sideAngle " + sideAngle + " " + sideAngle.toRoundedDegrees())
      val adjacentAngle = adjacent.toPolar.phi
      //logger.debug("adjacentAngle " + adjacentAngle + " " + adjacentAngle.toRoundedDegrees())
      val diff = sideAngle - adjacentAngle
      //logger.debug("diff " + diff + " " + diff.toRoundedDegrees())
      val absDiff = Math.abs(diff)
      //logger.debug("absDiff " + absDiff + " " + absDiff.toRoundedDegrees())
      val a = if (absDiff > TAU / 2) TAU - absDiff else absDiff
      //logger.debug("a " + a + " " + a.toRoundedDegrees())
      val condition = ((diff + TAU) % TAU) <~= (TAU / 2)
      val rotation  = sideAngle + (if (condition) a else 0)
      //logger.debug("rotation " + rotation + " " + rotation.toRoundedDegrees())
      val traslation = side.e
      RegularPgon.edgesNumberFrom(a).map(RegularPgon.ofEdges(_, l).toPolygon(rotation).sum(traslation))
    }

  }

  class Circle(val point: Point2D, r: Double) extends CoordsOps with SVG {

    /**
      *
      * @param style style sheet
      * @return
      */
    def toSVG(style: String = "stroke:green"): Elem =
      addStyle(<circle cx={point.x.roundAt().toString} cy={point.y.roundAt().toString} r={r.roundAt().toString} />,
               style)

    def sum(coords: Coords2D): Circle = new Circle(new Point2D(op(point.c, coords, _ + _)), r)

    def mult(coords: Coords2D): Circle = new Circle(new Point2D(op(point.c, coords, _ * _)), r)

    def sum(that: Point2D): Circle = sum(that.c)

    def move(x: Double, y: Double): Circle = sum(x, y)

    def scale(x: Double, y: Double): Circle = mult(x, y)

    def scale(xy: Double): Circle = scale(xy, xy)

  }

}
