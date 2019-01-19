package vision.id.tessella

import java.lang.Math.{atan2, hypot}
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
    def op(c1: Coords2D, c2: Coords2D, f: (Double, Double) => Double): Coords2D = (f(c1._1, c2._1), f(c1._2, c2._2))

    def equal(c1: Coords2D, c2: Coords2D): Boolean = (c1._1 ~= c2._1) && (c1._2 ~= c2._2)

    def comparison(c1: Coords2D, c2: Coords2D): Int =
      if (c1._1 ~= c2._1) {
        if (c1._2 ~= c2._2) 0
        else c1._2.compare(c2._2)
      } else c1._1.compare(c2._1)
  }

  class Points2D(val cs: List[Coords2D]) extends CoordsOps {

    /**
      * map to coords function combining points
      *
      * @param c coords
      * @param f operation
      * @return
      */
    private def map2(c: Coords2D, f: (Coords2D, Coords2D) => Coords2D): List[Coords2D] = cs.map(f(_, c))

    /**
      * map to coords function combining xs and ys
      *
      * @param c coords
      * @param f operation
      * @return
      */
    def mapOp(c: Coords2D, f: (Double, Double) => Double): List[Coords2D] = map2(c, op(_, _, f))

    def sum(c: Coords2D): Points2D = new Points2D(mapOp(c, _ + _))

    def diff(c: Coords2D): Points2D = new Points2D(mapOp(c, _ - _))

    def mult(c: Coords2D): Points2D = new Points2D(mapOp(c, _ * _))

    def min(c: Coords2D): Points2D = new Points2D(mapOp(c, Math.min))

    def max(c: Coords2D): Points2D = new Points2D(mapOp(c, Math.max))

    def sum(p: Point2D): Points2D = sum(p.c)

    def sum(lalpha: PointPolar): Points2D = sum(lalpha.toPoint2D)

    def diff(p: Point2D): Points2D = diff(p.c)

    def min(p: Point2D): Points2D = min(p.c)

    def max(p: Point2D): Points2D = max(p.c)

    def move(x: Double, y: Double): Points2D = sum(x, y)

    def move(xy: Double): Points2D = move(xy, xy)

    def scale(x: Double, y: Double): Points2D = mult(x, y)

    def scale(xy: Double): Points2D = scale(xy, xy)

    def equalCoords(that: Points2D): Boolean =
      this.cs.lengthCompare(that.cs.size) == 0 && cs.indices.forall(i => equal(this.cs(i), that.cs(i)))

    /**
      * get the lowest and higher points
      * delimiting the rectangular area covered by the points
      *
      * @return
      */
    def getMinMax: (Point2D, Point2D) = cs match {
      case Nil => (Point2D.origin, Point2D.origin)
      case _ =>
        val (cmin, cmax) = cs.foldLeft((orig, orig))({
          case ((min, max), c) => (op(min, c, Math.min), op(max, c, Math.max))
        })
        (new Point2D(cmin), new Point2D(cmax))
    }

    def toPoint2D(i: Int = 0): Point2D = new Point2D(cs(i))

    def toSegment2D(i: Int = 0): Segment2D = new Segment2D(cs(i), cs(i + 1))

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

    def sum(lalpha: PointPolar): Point2D = sum(lalpha.toPoint2D)

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

  class Label2D(c: Coords2D, s: String) extends Point2D(c) with SVG {

    //override def toString: String = "(" + s + ")" + super.toString

    def toSVG(style: String = "fill:red"): Elem =
      addStyle(<text x={x.roundAt().toString} y={y.roundAt().toString}>{s}</text>, style)

    override def sum(c1: Coords2D): Label2D = new Label2D(op(c, c1, _ + _), s)

    override def mult(c1: Coords2D): Label2D = new Label2D(op(c, c1, _ * _), s)

    override def sum(p: Point2D): Label2D = sum(p.c)

    override def move(x: Double, y: Double): Label2D = sum(x, y)

    override def scale(x: Double, y: Double): Label2D = mult(x, y)

    override def scale(xy: Double): Label2D = scale(xy, xy)

  }

  class Segment2D(cc: (Coords2D, Coords2D)) extends CoordsOps with SVG {

    val logger = Logger("SEGMENT2D")

    val (s, e): (Point2D, Point2D) = (new Point2D(cc._1), new Point2D(cc._2))

    require(s != e, "endpoints must be different")

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

    private def map2(c: Coords2D, f: (Coords2D, Coords2D) => Coords2D): (Coords2D, Coords2D) = (f(s.c, c), f(e.c, c))

    def mapOp(c: Coords2D, f: (Double, Double) => Double): (Coords2D, Coords2D) = map2(c, op(_, _, f))

    def sum(c: Coords2D): Segment2D = new Segment2D(mapOp(c, _ + _))

    def mult(c: Coords2D): Segment2D = new Segment2D(mapOp(c, _ * _))

    def sum(p: Point2D): Segment2D = sum(p.c)

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
      * diagonal from bottom left to top right of the rectangular area occupied by the two segments
      *
      * @param that segment
      * @return
      */
    def rect(that: Segment2D): Segment2D = Segment2D.fromPoint2Ds(this.min.min(that.min.c), this.max.max(that.max.c))

    /**
      * check if the given point is endpoint of the segment
      *
      * @param p point
      * @return
      */
    def isJoining(p: Point2D): Boolean = s == p || e == p

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
      * @param p point
      * @return > 0 for p left of the line passing by segment, == 0 on the line, < 0 right of the line
      */
    def isLeft(p: Point2D): Double = (e.x - s.x) * (p.y - s.y) - (p.x - s.x) * (e.y - s.y)

    /**
      * flip horizontally along the vertical line with given x
      *
      * @param coordx vertical line coord
      * @return
      */
    def flipH(coordx: Double): Segment2D = Segment2D.fromPoint2Ds(s.flipH(coordx), e.flipH(coordx))

    /**
      * rotate around centre of given angle
      *
      * @param angle  angle in radians
      * @param centre point
      * @return
      */
    def rotate(angle: Double, centre: Point2D = Point2D.origin): Segment2D =
      Segment2D.fromPoint2Ds(s.rotate(angle, centre), e.rotate(angle, centre))

  }

  object Segment2D {

    def fromPoint2Ds(sp: Point2D, ep: Point2D): Segment2D = new Segment2D(sp.c, ep.c)

  }

  class Polyline2D(cs: List[Coords2D]) extends Points2D(cs) {

    def toPointsAttr: String =
      cs.map({ case (x, y) => x.roundAt().toString + "," + y.roundAt().toString }).mkString(" ")

    /**
      *
      * @param style style sheet
      * @return
      */
    def toSVG(style: String = "fill:none;stroke:black;stroke-width:3"): Elem =
      <polyline style={style} points={toPointsAttr}/>

  }

  class Polygon(cs: List[Coords2D]) extends Polyline2D(cs) with ListUtils {

    /**
      *
      * @param style style sheet
      * @return
      */
    override def toSVG(style: String = "fill:none;stroke:green;stroke-width:3"): Elem =
      <polygon style={style} points={toPointsAttr}/>

    override def sum(c: Coords2D): Polygon = new Polygon(mapOp(c, _ + _))

    override def mult(c: Coords2D): Polygon = new Polygon(mapOp(c, _ * _))

    override def sum(p: Point2D): Polygon = sum(p.c)

    override def move(x: Double, y: Double): Polygon = sum(x, y)

    override def scale(x: Double, y: Double): Polygon = mult(x, y)

    override def scale(xy: Double): Polygon = scale(xy, xy)

    def toSegments2D: List[Segment2D] = cs.circularSliding(2).toList.map(l => new Segment2D(l.safeHead, l(1)))

    def includes(p: Point2D): Boolean = Polygon.segmentsIncludes(toSegments2D, p)

    /**
      * @note valid for simple polygons only
      * @param s segment to be checked for inclusion
      * @return
      */
    def includes(s: Segment2D): Boolean = Polygon.segmentsIncludes(toSegments2D, s)

    /**
      * @see https://gis.stackexchange.com/questions/22739/finding-center-of-geometry-of-object/22744#22744
      */
    def barycenter: Point2D = {
      val s      = cs.size
      val (x, y) = cs.foldLeft((0.0, 0.0))({ case ((sumx, sumy), (x, y)) => (sumx + x, sumy + y) })
      new Point2D(x / s, y / s)
    }
  }

  object Polygon extends MathUtils {

    val logger = Logger("POLYGON")

    /**
      * check if point is included in polygon represented by segments
      * see C++ code http://geomalgorithms.com/a03-_inclusion.html
      *
      * @param p point to be checked for inclusion
      * @return
      */
    def segmentsIncludes(segments: List[Segment2D], p: Point2D): Boolean =
      segments.foldLeft(0)(
        (wn, segm) =>
          wn + (
            if (segm.s.y <= p.y) {
              // an upward crossing and p left of edge
              if (segm.e.y > p.y && segm.isLeft(p) > 0) 1 // have a valid up intersect
              else 0
            } else {
              // a downward crossing and p right of edge
              if (segm.e.y <= p.y && segm.isLeft(p) < 0) -1 // have a valid down intersect
              else 0
            }
        )) != 0

    private def segmentsIncludesEndpoints(segments: List[Segment2D], s: Segment2D): Boolean =
      segmentsIncludes(segments, s.s) && segmentsIncludes(segments, s.e)

    def segmentsIncludes(segments: List[Segment2D], s: Segment2D): Boolean =
      segmentsIncludesEndpoints(segments, s) && segments.forall(!s.isIntersecting(_))

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

  class Circle(c: Coords2D, r: Double) extends Point2D(c) with SVG {

    /**
      *
      * @param style style sheet
      * @return
      */
    def toSVG(style: String = "stroke:green"): Elem =
      addStyle(<circle cx={x.roundAt().toString} cy={y.roundAt().toString} r={r.roundAt().toString} />, style)

    override def sum(c1: Coords2D): Circle = new Circle(op(c, c1, _ + _), r)

    override def mult(c1: Coords2D): Circle = new Circle(op(c, c1, _ * _), r)

    override def sum(p: Point2D): Circle = sum(p.c)

    override def move(x: Double, y: Double): Circle = sum(x, y)

    override def scale(x: Double, y: Double): Circle = mult(x, y)

    override def scale(xy: Double): Circle = scale(xy, xy)

  }

}
