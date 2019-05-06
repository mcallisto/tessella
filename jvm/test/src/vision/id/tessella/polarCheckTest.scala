package vision.id.tessella

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec

import vision.id.tessella.Tau.TAU
import vision.id.tessella.Polar.{PointPolar, UnitPolyline}
import vision.id.tessella.Cartesian2D.Point2D

class polarCheckTest extends FlatSpec with Checkers with CoordGenerators {

  val polars: Gen[PointPolar] = for {
    p <- genPoint2D
  } yield p.toPolar

  "A polar point angle" must "always be ≥ 0 and ≤ TAU" in {
    check(forAll(polars)(polar => polar.phi >= 0.0 && polar.phi <= TAU))
  }

  "A polar point" must "be equal to another point with a slightly different radius" in {
    val pointComponents: Gen[(PointPolar, Double)] = for {
      polar <- polars
      diffr <- withinPrecision
    } yield (polar, diffr)

    check(forAll(pointComponents) {
      case (polar, diffr) => polar === polar.sum((diffr, 0.0))
    })
  }

  val notTooDistant: Gen[PointPolar] = polars suchThat (_.r.abs < oneMillion)

  "A polar point distant from origin" can "be different from itself after many full angle rotations" in {
    val p0        = new PointPolar((1.9230665288756824E8, 1.5707963267948966))
    val p0rotated = p0.rotate(3 * TAU)
    assert(p0.r === p0rotated.r)
    assert(p0.phi ~= p0rotated.phi)
    assert(p0 !== p0rotated)
    val (x0, y0)               = p0.toCartesianCoords2D
    val (x0rotated, y0rotated) = p0rotated.toCartesianCoords2D
    assert(!(x0 ~= x0rotated))
    assert(x0 ~= (x0rotated, stdPrecision * 100))
    assert(y0 ~= y0rotated)

    val p1        = new PointPolar((846230.2958191511, 3.141592653589793))
    val p1rotated = p1.rotate(81 * TAU)
    assert(p1.r === p1rotated.r)
    assert(p1.phi ~= p1rotated.phi)
    assert(p1 !== p1rotated)
    val (x1, y1)               = p1.toCartesianCoords2D
    val (x1rotated, y1rotated) = p1rotated.toCartesianCoords2D
    assert(x1 ~= x1rotated)
    assert(!(y1 ~= y1rotated))
    assert(y1 ~= (y1rotated, stdPrecision * 100))
  }

  "A polar point not too distant from origin" must "be always equal to itself when double reflected" in {
    val p = new PointPolar((2.844362358252713E8, TAU / 4))
    assert(p.reflect.reflect === p)
    check(forAll(notTooDistant)(polar => polar === polar.reflect.reflect))
  }

  it must "be equal to itself with negative radius and inverted angle" in {
    check(forAll(notTooDistant)(polar => polar === new PointPolar(-polar.r, polar.phi).rotate(TAU / 2)))
  }

  it must "be always equal to itself no matter how many full angle rotations" in {
    val pointComponents: Gen[(PointPolar, Int)] = for {
      polar     <- notTooDistant
      rotations <- Gen.choose(-100, 100)
    } yield (polar, rotations)

    check(forAll(pointComponents) {
      case (polar, rotations) => polar === polar.rotate(rotations * TAU)
    })
  }

  val close = new PointPolar((0.0, 1.0))

  "A polar point close to origin" must "be equal to another point with a slightly different angle" in {
    check(forAll(withinPrecision) { angle =>
      close == close.rotate(angle)
    })
  }

  val unitPolars: Gen[PointPolar] = for {
    p <- genPoint2D
  } yield new PointPolar((1.0, p.toPolar.phi))

  "A unit polyline and its reflection" must "grow keeping the same distance from origin" in {
    val polylines: Gen[UnitPolyline] = for {
      points <- Gen.containerOf[List, PointPolar](unitPolars)
    } yield new UnitPolyline(points)

    check(forAll(polylines) { polyline =>
      polyline
        .toPolyline2D()
        .points
        .zip(polyline.reflect.toPolyline2D().points)
        .forall({
          case (point1, point2) =>
            point1.distanceFrom(Point2D.origin) ~= point2.distanceFrom(Point2D.origin)
        })
    })
  }

//  "Two ~= polar points" must "be converted into two ~= cartesian points" in {
//    val p0 = new PointPolar((1.9230665288756824E8, 1.5707963267948966))
//    val almostp0 = p0.rotate(3 * TAU)
//    assert(!(p0.toCartesianCoords2D === almostp0.toCartesianCoords2D))
//    println(p0)
//    println(almostp0)
//    println(p0.toPoint2D)
//    println(almostp0.toPoint2D)
//    println(p0.toPoint2D.x - almostp0.toPoint2D.x)
//    println(p0.toPoint2D.y - almostp0.toPoint2D.y)
//    assert(p0.toPoint2D.y ~= almostp0.toPoint2D.y)
//    assert(p0.toPoint2D.x ~= almostp0.toPoint2D.x)
//
//    val polars: Gen[(PointPolar, PointPolar)] = for {
//      p <- genPoint2D
//      polar = p.toPolar
//      diffr <- withinPrecision
//      diffphi <- withinPrecision
//    } yield (polar, polar.sum((diffr, diffphi)))
//
//    check(forAll(polars) { case (polar, almostEqual) => polar === almostEqual })
//  }

}
