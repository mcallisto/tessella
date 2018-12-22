package vision.id.tessella

import java.lang.Math.sqrt

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import vision.id.tessella.Tau.TAU
import vision.id.tessella.Cartesian2D._
import vision.id.tessella.Polar.{PointPolar, UnitRegularPgon}

class cartesian2DTest extends FlatSpec with TryUtils {

  val cart: Point2D   = new Point2D((1.0, 1.0))
  val pol: PointPolar = new PointPolar(sqrt(2.0), TAU / 8)

  "Two coords" must "be ordered by first element and then second" in {
    val c0 = (0.0, 1.0)
    val c1 = (0.0, 0.0)
    assert(List(c0, c1).sorted === List(c1, c0))
    assert(List(c1, c0).sorted === List(c1, c0))
    val c2 = (0.0, 1.0)
    val c3 = (2.0, 0.0)
    assert(List(c2, c3).sorted === List(c2, c3))
    assert(List(c3, c2).sorted === List(c2, c3))
    val c1p = (0.000000001, 0.0)
    assert(List(c0, c1p).sorted !== List(c1p, c0))
    assert(List(c1p, c0).sorted !== List(c1p, c0))
  }

  "A polar coord" can "be converted into a cartesian point" in {
    assert(pol.toPoint2D === cart)
  }

  it must "be in the cartesian pos quadrant if angle in first half 𝜋" in {
    val c = new PointPolar(1.0, TAU / 6).toPoint2D
    assert(c.x > 0 && c.y > 0)
  }

  it must "be in the x-neg and y-pos quadrant if angle in second half 𝜋" in {
    val c = new PointPolar(1.0, TAU / 3).toPoint2D
    assert(c.x < 0 && c.y > 0)
  }

  it can "be converted into a segment starting from origin" in {
    assert(pol.toSegment2D === Segment2D.fromPoint2Ds(Point2D.origin, cart))
  }

  "A point" can "be converted into polar coords from origin" in {
    assert(cart.toPolar === pol)
  }

  it can "be moved" in {
    assert(Point2D.origin.move(cart.x, cart.y) === cart)
    assert(Point2D.origin.sum(pol) === cart)
  }

  it can "be flipped horizontally around a vertical line" in {
    val p = new Point2D(3.0, 2.0)
    assert(p.flipH(1.0) === new Point2D(-1.0, 2.0))
    assert(p.flipH(2.0).flipH(2.0) === p)
  }

  it can "be rotated of a given angle around a centre" in {
    val p = new Point2D(3.0, 2.0)
    assert(p.rotate(TAU / 4, new Point2D(3.0, 3.0)) === new Point2D(4.0, 3.0))
    assert(p.rotate(TAU / 2, new Point2D(3.0, 3.0)) === new Point2D(3.0, 4.0))
    assert(p.rotate(TAU / 4, p) === p)
  }

  "Two points" must "be ordered for x and then for y" in {
    val p0 = new Point2D(0.0, 1.0)
    val p1 = new Point2D(0.0, 0.0)
    assert(List(p0, p1).sorted === List(p1, p0))
    assert(List(p1, p0).sorted === List(p1, p0))
  }

  they must "be ordered for x and then for y (xs can be approx equal)" in {
    val p0 = new Point2D(0.0, 1.0)
    val p1 = new Point2D(0.000000001, 0.0)
    assert(List(p0, p1).sorted === List(p1, p0))
    assert(List(p1, p0).sorted === List(p1, p0))
  }

  they must "be ordered as they are, if equal coordinates)" in {
    val p0 = new Point2D(0.0, 1.0)
    val p1 = new Point2D(0.000000001, 1.0)
    assert(List(p0, p1).sorted === List(p0, p1))
    assert(List(p1, p0).sorted === List(p1, p0))
  }

  they can "have a perp product" in {
    val p = new Point2D(3.0, 2.0)
    val b = new Point2D(1.0, 1.0)
    assert(p.perp(b) === 1.0)
    assert(b.perp(p) === -1.0)
  }

  val horizontal: Segment2D = new Segment2D((1.0, 2.0), (-3.0, 2.0))
  val vertical: Segment2D   = new Segment2D((1.0, -4.0), (1.0, 5.0))
  val diagonal: Segment2D   = new Segment2D((-1.0, 2.0), (1.0, 0.0))

  "A segment" can "be moved as starting from origin" in {
    val h = horizontal.fromOrigin
    assert(h === new Point2D(-4.0, 0.0))
    val v = vertical.fromOrigin
    assert(v === new Point2D(0.0, 9.0))
    val d = diagonal.fromOrigin
    assert(d === new Point2D(2.0, -2.0))
  }

  the[IllegalArgumentException] thrownBy new Segment2D((1.0, 1.0), (1.0, 1.0)) should have message
    "requirement failed: endpoints must be different"

  it can "be moved" in {
    assert(horizontal.move(1.0, 1.0) === new Segment2D((2.0, 3.0), (-2.0, 3.0)))
  }
  it can "be converted into polar coords from origin" in {
    val s = Segment2D.fromPoint2Ds(Point2D.origin, cart)
    assert(s.toPolar === pol)
  }

  val hin  = new Segment2D((-4.0, 2.0), (0.0, 2.0))
  val hout = new Segment2D((4.0, 2.0), (10.0, 2.0))

  it must "have a x and y length" in {
    val d1 = hin.diff
    assert(d1 === new Point2D(-4.0, 0.0))
    val d2 = hout.diff
    assert(d2 === new Point2D(-6.0, 0.0))
  }

  it can "be flipped horizontally around a vertical line" in {
    val s = new Segment2D((3.0, 2.0), (0.0, -5.0))
    assert(s.flipH(1.0) === new Segment2D((-1.0, 2.0), (2.0, -5.0)))
    assert(s.flipH(2.0).flipH(2.0) === s)
  }

  it can "be rotated of a given angle around a centre" in {
    val s = new Segment2D((3.0, 2.0), (0.0, -5.0))
    assert(s.rotate(TAU / 4, new Point2D(3.0, 3.0)) === new Segment2D((4.0, 3.0), (11.0, 0.0)))
    assert(s.rotate(TAU / 2, new Point2D(3.0, 3.0)) === new Segment2D((3.0, 4.0), (6.0, 11.0)))
  }

  "A label" can "be moved" in {
    val l = new Label2D((-0.5, 1.0), "x")
    val n = l.sum(new Point2D(1.0, 1.0))
    assert(n.x === 0.5)
    assert(n.y === 2.0)
  }

  "A regular triangle" can "be created from two adjacent sides" in {
    val side1 = new Segment2D((-2.5, -0.8660254), (-2.0, 0.0))
    val side2 = new Segment2D((-2.5, -0.8660254), (-3.0, 0.0))
    val p     = Polygon.createRegularFrom(side1, side2).safeGet
    assert(
      p.toSegments2D.map(_.toString) === List("[{-2.0:0.0}|{-3.0:0.0}]",
                                              "[{-3.0:0.0}|{-2.5:-0.8660254}]",
                                              "[{-2.5:-0.8660254}|{-2.0:0.0}]"))
  }

  it can "be created from another two adjacent sides" in {
    val side1 = new Segment2D((-1.0, 0.0), (0.0, 0.0))
    val side2 = new Segment2D((-1.0, 0.0), (-0.5, 0.8660254))
    val p     = Polygon.createRegularFrom(side1, side2).safeGet
    assert(
      p.toSegments2D.map(_.toString) === List("[{0.0:0.0}|{-0.5:0.8660254}]",
                                              "[{-0.5:0.8660254}|{-1.0:0.0}]",
                                              "[{-1.0:0.0}|{0.0:0.0}]"))
  }

  it can "be created from another two adjacent sides swapped" in {
    val side1 = new Segment2D((-1.0, 0.0), (-0.5, 0.8660254))
    val side2 = new Segment2D((-1.0, 0.0), (0.0, 0.0))
    val p     = Polygon.createRegularFrom(side1, side2).safeGet
    assert(
      p.toSegments2D.map(_.toString) === List("[{-0.5:0.8660254}|{-1.0:0.0}]",
                                              "[{-1.0:0.0}|{0.0:0.0}]",
                                              "[{0.0:0.0}|{-0.5:0.8660254}]"))
  }

  "A square" can "be created from two adjacent sides" in {
    val side1 = new Segment2D((3.0, -1.0), (1.0, -2.5))
    val side2 = new Segment2D((3.0, -1.0), (1.5, 1.0))
    val p     = Polygon.createRegularFrom(side1, side2).safeGet
    assert(
      p.toSegments2D.map(_.toString) === List("[{1.0:-2.5}|{3.0:-1.0}]",
                                              "[{3.0:-1.0}|{1.5:1.0}]",
                                              "[{1.5:1.0}|{-0.5:-0.5}]",
                                              "[{-0.5:-0.5}|{1.0:-2.5}]"))
  }

  it can "be created from the same two adjacent sides swapped" in {
    val side1 = new Segment2D((3.0, -1.0), (1.5, 1.0))
    val side2 = new Segment2D((3.0, -1.0), (1.0, -2.5))
    val p     = Polygon.createRegularFrom(side1, side2).safeGet
    assert(
      p.toSegments2D.map(_.toString) === List("[{1.5:1.0}|{-0.5:-0.5}]",
                                              "[{-0.5:-0.5}|{1.0:-2.5}]",
                                              "[{1.0:-2.5}|{3.0:-1.0}]",
                                              "[{3.0:-1.0}|{1.5:1.0}]"))
  }

  "An hexagon" can "be created from two adjacent sides" in {
    val side1 = new Segment2D((-0.5, 0.8660254), (0.0, 0.0))
    val side2 = new Segment2D((-0.5, 0.8660254), (-1.5, 0.8660254))
    val p     = Polygon.createRegularFrom(side1, side2).safeGet
    assert(
      p.toSegments2D.map(_.toString) === List(
        "[{0.0:0.0}|{-0.5:0.8660254}]",
        "[{-0.5:0.8660254}|{-1.5:0.8660254}]",
        "[{-1.5:0.8660254}|{-1.99999999:-1.0E-8}]",
        "[{-1.99999999:-1.0E-8}|{-1.49999999:-0.86602541}]",
        "[{-1.49999999:-0.86602541}|{-0.5:-0.8660254}]",
        "[{-0.5:-0.8660254}|{0.0:0.0}]"
      ))
  }

  "A square" must "have a barycenter of its vertices" in {
    assert(UnitRegularPgon.ofSides(4).toPolygon().barycenter === new Point2D(-0.5, 0.5))
  }
}
