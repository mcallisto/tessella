package vision.id.tessella

import java.lang.Math.sqrt

import org.scalatest.FlatSpec

import vision.id.tessella.Cartesian2D._
import vision.id.tessella.Polar.PointPolar
import vision.id.tessella.Tau.TAU

class intersectionTest extends FlatSpec with ListUtils with TryUtils {

  val cart: Point2D   = new Point2D((1.0, 1.0))
  val pol: PointPolar = new PointPolar(sqrt(2.0), TAU / 8)

  "Two points" can "be checked for the intersection of their unit circles" in {
    assert(new Point2D(0.0, 0.0).unitContacts(new Point2D(0.0, 0.0)).isFailure)
    assert(
      new Point2D(0.0, 0.0).unitContacts(new Point2D(2.0, 0.0)).safeGet.map(_.toString) === List(
        "{1.0:0.0}"
      ))
    assert(
      new Point2D(0.0, 0.0).unitContacts(new Point2D(-2.0, 0.0)).safeGet.map(_.toString) === List(
        "{-1.0:0.0}"
      ))
    assert(
      new Point2D(0.0, 0.0).unitContacts(new Point2D(0.0, -2.0)).safeGet.map(_.toString) === List(
        "{0.0:-1.0}"
      ))
    assert(new Point2D(0.0, 0.0).unitContacts(new Point2D(3.0, 0.0)).safeGet === List())
    assert(
      new Point2D(0.0, 0.0).unitContacts(new Point2D(1.0, 0.0)).safeGet.map(_.toString) === List(
        "{0.5:-0.8660254}",
        "{0.5:0.8660254}"
      ))
    assert(
      new Point2D(0.0, 0.0).unitContacts(new Point2D(-1.0, -1.0)).safeGet.map(_.toString) === List(
        "{-1.0:0.0}",
        "{0.0:-1.0}"
      ))
    assert(
      new Point2D(2.0, 0.0).unitContacts(new Point2D(1.0, 1.0)).safeGet.map(_.toString) === List(
        "{2.0:1.0}",
        "{1.0:0.0}"
      ))
  }

  they must "have intersections ordered" in {
    val a  = new Point2D(0.0, 0.0)
    val b  = new Point2D(1.0, 1.0)
    val u1 = a.unitContacts(b).safeGet
    val u2 = b.unitContacts(a).safeGet
    assert(u1.onlyTwoElements((f1, s1) => u2.onlyTwoElements((f2, s2) => f1 === s2 && f2 === s1)))
  }

  "Three points" can "be checked if they all have a fourth at unit distance" in {
    assert(
      new Point2D(1.0, 1.0).getFourth(new Point2D(0.0, 0.0), new Point2D(2.0, 0.0)).safeGet.toString ===
        "{1.0:0.0}")
    assert(
      new Point2D(0.0, 0.0).getFourth(new Point2D(2.0, 0.0), new Point2D(1.0, 1.0)).safeGet.toString ===
        "{1.0:0.0}")
  }

  val horizontal: Segment2D = Segment2D.fromCoords2D((1.0, 2.0), (-3.0, 2.0))
  val vertical: Segment2D   = Segment2D.fromCoords2D((1.0, -4.0), (1.0, 5.0))
  val diagonal: Segment2D   = Segment2D.fromCoords2D((-1.0, 2.0), (1.0, 0.0))

  val hin: Segment2D  = Segment2D.fromCoords2D((-4.0, 2.0), (0.0, 2.0))
  val hout: Segment2D = Segment2D.fromCoords2D((4.0, 2.0), (10.0, 2.0))

  "Two segments" can "not intersect" in {
    val s1: Segment2D = Segment2D.fromCoords2D((-1.0, 4.0), (-1.0, 3.0))
    val s2: Segment2D = Segment2D.fromCoords2D((-1.0, 1.0), (-1.0, 0.0))
    assert(!s1.isIntersecting(s2))
    assert(!s2.isIntersecting(s1))
    val s3: Segment2D = Segment2D.fromCoords2D((-1.0, 3.0), (-1.0, 2.0))
    val s4: Segment2D = Segment2D.fromCoords2D((-1.0, 1.0), (-1.0, 0.0))
    assert(!s3.isIntersecting(s4))
    assert(!s4.isIntersecting(s3))
    val s5: Segment2D = Segment2D.fromCoords2D((6.5, 4.33012702), (6.0, 5.19615242))
    val s6: Segment2D = Segment2D.fromCoords2D((5.0, 6.92820323), (4.5, 7.79422863))
    assert(!s5.isIntersecting(s6))
    assert(!s6.isIntersecting(s5))
  }

  they must "intersect if they share a segment" in {
    assert(horizontal.isIntersecting(hin))
    assert(!horizontal.isIntersecting(hout))
  }

  they can "be joined at one endpoint" in {
    val joind: Segment2D = Segment2D.fromCoords2D((-1.0, 2.0), (2.0, 3.0))
    assert(diagonal.isJoining(joind))
  }

  they can "intersect" in {
    val s1: Segment2D = Segment2D.fromCoords2D((-0.8660254, -0.5), (-0.8660254, 0.5))
    val s2: Segment2D = Segment2D.fromCoords2D((-1.0, 0.0), (0.0, 0.0))
    val s3: Segment2D = Segment2D.fromCoords2D((-0.8660254, 0.5), (-1.8660254, 0.5))
    val s4: Segment2D = Segment2D.fromCoords2D((-1.5, 0.8660254), (-1.0, 0.0))
    val s5: Segment2D = Segment2D.fromCoords2D((-1.5, 0.0), (0.5, 0.0))
    val s6: Segment2D = Segment2D.fromCoords2D((-2.0, 0.0), (-1.0, 0.0))
    val s7: Segment2D = Segment2D.fromCoords2D((-1.0, 0.0), (-1.0, -1.0))
    assert(s1.isIntersecting(s2) === true)
    assert(s3.isIntersecting(s4) === true)
    assert(s2.isIntersecting(s5) === true)
    assert(s2.isIntersecting(s6) === true)
    assert(s2.isIntersecting(s7) === true)
  }

}
