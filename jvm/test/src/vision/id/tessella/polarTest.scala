package vision.id.tessella

import java.lang.Math.{sqrt, toRadians}

import org.scalatest.FlatSpec

import vision.id.tessella.Tau.TAU
import vision.id.tessella.Cartesian2D.Point2D
import vision.id.tessella.Polar._

class polarTest extends FlatSpec with MathUtils with TryUtils {

  "A polar point" must "be normalized" in {
    assert(new PointPolar(1.0, 0.0).phi ~= 0.0)
    assert(new PointPolar(1.0, TAU / 4).phi ~= (TAU / 4))
    assert(new PointPolar(1.0, -TAU / 4).phi ~= (TAU / 4 * 3))
    assert(new PointPolar(1.0, -TAU * 5 / 2).phi ~= (TAU / 2))
    assert(new PointPolar(1.0, -TAU * 2).phi ~= 0.0)
  }

  it can "be printed" in {
    assert(new PointPolar(1.0, TAU / 4).toString === "1.0~>90.0°")
    assert(new PointPolar(-1.0, TAU / 4 + TAU).toString === "-1.0~>90.0°")
  }

  it can "be reflected" in {
    assert(new PointPolar(1.0, 0.0).reflect === new PointPolar(1.0, 0.0))
    assert(new PointPolar(1.0, TAU / 2).reflect === new PointPolar(1.0, TAU / 2))
    assert(new PointPolar(1.0, TAU / 4).reflect === new PointPolar(1.0, TAU / 4 * 3))
  }

  val empty: Polyline                = new Polyline(List())
  val oneLine: Polyline              = new Polyline(List(new PointPolar(sqrt(2.0), TAU / 8)))
  val closeTriangle: Polyline        = new Polyline(List.fill(3)(new PointPolar(1.0, TAU / 3)))
  val squarePolars: List[PointPolar] = List.fill(4)(new PointPolar(1.0, TAU / 4))
  val closeSquare: Polyline          = new Polyline(squarePolars)
  val closeHex: Polyline             = new Polyline(List.fill(6)(new PointPolar(1.0, TAU / 6)))
  val star: Polyline                 = new Polyline(List.fill(7)(new PointPolar(1.0, toRadians(177.0))))

  "A polyline" must "have an end" in {
    assert(oneLine.end === new Point2D(1.0, 1.0))
    assert(closeSquare.end === Point2D.origin)
    assert(closeTriangle.end === Point2D.origin)
    assert(closeHex.end === Point2D.origin)
  }

  it must "NOT have a line of length 0" in {
    assertThrows[IllegalArgumentException] {
      new Polyline(squarePolars.take(3) :+ new PointPolar(0.0, TAU / 4))
    }
  }

  it can "be converted into cartesian points" in {
    assert(oneLine.toPoint2Ds().map(_.toString) == List("{0.0:0.0}", "{1.0:1.0}"))
    assert(
      closeTriangle.toPoint2Ds().map(_.toString) == List("{0.0:0.0}", "{-0.5:0.8660254}", "{-1.0:0.0}", "{0.0:0.0}"))
  }

  it can "be converted into cartesian segments" in {
    assert(oneLine.toSegments2D().map(_.toString) == List("[{0.0:0.0}|{1.0:1.0}]"))
    assert(
      closeTriangle.toSegments2D().map(_.toString) == List("[{0.0:0.0}|{-0.5:0.8660254}]",
                                                           "[{-0.5:0.8660254}|{-1.0:0.0}]",
                                                           "[{-1.0:0.0}|{0.0:0.0}]"))
    assert(
      closeSquare.toSegments2D().map(_.toString) == List(
        "[{0.0:0.0}|{0.0:1.0}]",
        "[{0.0:1.0}|{-1.0:1.0}]",
        "[{-1.0:1.0}|{-1.0:0.0}]",
        "[{-1.0:0.0}|{0.0:0.0}]"
      ))
  }

  it can "be simplified" in {
    assert(empty.simplify.equalCoords(empty))
    assert(oneLine.simplify.equalCoords(oneLine))
    assert(closeTriangle.simplify.equalCoords(closeTriangle))
    val z  = new Polyline(List.fill(3)(new PointPolar(1.0, 0.0)))
    val l  = new PointPolar(1.0, TAU / 4)
    val z1 = new Polyline(List.fill(3)(new PointPolar(1.0, 0.0)) :+ l)
    val z2 = new Polyline(l +: List.fill(3)(new PointPolar(1.0, 0.0)))
    assert(z.simplify.equalCoords(new Polyline(List(new PointPolar(3.0, 0.0)))))
    assert(z1.simplify.equalCoords(new Polyline(List(new PointPolar(3.0, 0.0), l))))
    assert(z2.simplify.equalCoords(new Polyline(List(l, new PointPolar(3.0, 0.0)))))

  }

  it can "have intersections" in {
    assert(oneLine.selfIntersections.map(_.toString()) === List())
    assert(closeTriangle.selfIntersections.map(_.toString()) === List())
    assert(star.selfIntersections.size === 15)
  }

  it can "be checked if has at least one intersection" in {
    assert(!oneLine.isSelfIntersecting)
    assert(!closeTriangle.isSelfIntersecting)
    assert(star.isSelfIntersecting)
  }

  it can "be reflected" in {
    assert(new Polyline(squarePolars).reflect.equalCoords(new Polyline(List.fill(4)(new PointPolar(1.0, TAU / 4 * 3)))))
  }

  "A p-gon" must "have more than 2 sides" in {
    assertThrows[IllegalArgumentException] {
      new Pgon(List(new PointPolar(3.0, TAU / 6), new PointPolar(4.0, TAU / 4)))
    }
  }

  it must "have the same number of points and segments" in {
    val sq = new Pgon(squarePolars)
    assert(sq.toSegments2D().size === sq.toPoint2Ds().size)
  }

  it must "NOT have any polar angle equal to 180°" in {
    val one               = new PointPolar(1.0, TAU / 4)
    val backtrackedSquare = List(one, new PointPolar(1.0, TAU / 2), one, one, one, one)
    // it is ok for a Pgon
    assert(new Pgon(backtrackedSquare).isInstanceOf[Pgon] === true)
    // but not for a SimplePgon
    assertThrows[IllegalArgumentException] {
      new SimplePgon(backtrackedSquare)
    }
  }

  val unitRegTriangle: UnitRegularPgon = UnitRegularPgon.ofSides(3)
  val unitSquare: UnitRegularPgon      = UnitRegularPgon.ofSides(4)
  val unitHex: UnitRegularPgon         = UnitRegularPgon.ofSides(6)

  "An equilateral triangle" should "have an internal angle of 60°" in {
    assert(unitRegTriangle.alpha ~= (TAU / 6))
    assert(unitRegTriangle.alpha.toDegrees.roundAt() === 60.0)
  }

  it should "have a polar angle of 120°" in {
    assert(unitRegTriangle.phi ~= (TAU / 3))
    assert(unitRegTriangle.phi.toDegrees.roundAt() === 120.0)
  }

  "A square" should "have an internal angle of 90°" in {
    assert(unitSquare.alpha ~= (TAU / 4))
    assert(unitSquare.alpha.toDegrees.roundAt() === 90.0)
  }

  it should "have a polar angle of 90°" in {
    assert(unitSquare.phi ~= (TAU / 4))
    assert(unitSquare.phi.toDegrees.roundAt() === 90.0)
  }

  "A regular hexagon" should "have an internal angle of 120°" in {
    assert(unitHex.alpha ~= (TAU / 3))
    assert(unitHex.alpha.toDegrees.roundAt() === 120.0)
  }

  it should "have a polar angle of 60°" in {
    assert(unitHex.phi ~= (TAU / 6))
    assert(unitHex.phi.toDegrees.roundAt() === 60.0)
  }

  "A unit square" should "have an inradius of 0.5" in {
    assert(unitSquare.r ~= 0.5)
  }

  it should "have a circumradius of half √2" in {
    assert(unitSquare.R ~= (sqrt(2.0) / 2))
  }

  it should "have an area of 1" in {
    assert(unitSquare.area ~= 1.0)
  }

  "A unit hexagon" should "have an area of 2.598076211" in {
    assert(unitHex.area ~= 2.598076211)
  }

  "An internal angle of 90°" can "be ascribed to a regular polygon of 4 sides" in {
    assert(RegularPgon.sides(TAU / 4).safeGet === 4)
  }

  "An internal angle of 120°" can "be ascribed to a regular polygon of 6 sides" in {
    assert(RegularPgon.sides(TAU / 3).safeGet === 6)
    assert(RegularPgon.sides(2.0943951042854145).safeGet === 6)
  }

  "An internal angle of 36°" can "NOT be ascribed to any regular polygon" in {
    assert(RegularPgon.sides(TAU / 10).isFailure)
  }

}
