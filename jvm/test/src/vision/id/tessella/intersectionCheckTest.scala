package vision.id.tessella

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary.arbitrary

import vision.id.tessella.Cartesian2D.Segment2D

class intersectionCheckTest extends FlatSpec with Checkers with CoordGenerators {

  "Two identical segments" must "be joined" in {
    check(forAll(genSegment2D) { s =>
      s.isJoining(s)
    })
  }

  they must "intersect" in {
    check(forAll(genSegment2D) { s =>
      s.isIntersecting(s)
    })
  }

  "Two segments shaping a └" must "intersect and be joined" in {
    Segment2D.fromCoords2D((2.0, 0.0), (0.0, 0.0)).isJoining(Segment2D.fromCoords2D((0.0, 0.0), (0.0, 2.0)))
  }

  val twoJoinedSegments: Gen[(Segment2D, Segment2D)] = for {
    s <- genSegment2D
    p <- genPoint2D suchThat (!_.equals(s.s))
  } yield (s, new Segment2D(s.s, p))

  "Two segments sharing an endpoint" must "be joined" in {
    check(forAll(twoJoinedSegments) { case (s1, s2) => s1.isJoining(s2) })
  }

  they must "intersect" in {
    check(forAll(twoJoinedSegments) { case (s1, s2) => s1.isIntersecting(s2) })
  }

  val tSegments: Gen[(Segment2D, Segment2D)] = for {
    x <- arbitrary[Double]
  } yield
    (
      Segment2D.fromCoords2D((Double.MinValue, Double.MaxValue), (Double.MaxValue, Double.MaxValue)),
      Segment2D.fromCoords2D((x, Double.MinValue), (x, Double.MaxValue))
    )

  "Two segments shaping a ┬" must "intersect but not be joined" in {
    Segment2D
      .fromCoords2D((-1.0, 0.0), (1.0, 0.0))
      .isNotJoiningButIntersecting(Segment2D.fromCoords2D((0.0, 0.0), (0.0, -2.0)))
    check(forAll(tSegments) { case (s1, s2) => s1.isNotJoiningButIntersecting(s2) })
  }

  val pSegments: Gen[(Segment2D, Segment2D)] = for {
    p <- genPoint2D
  } yield
    (
      Segment2D.fromCoords2D((Double.MinValue, p.y), (Double.MaxValue, p.y)),
      Segment2D.fromCoords2D((p.x, Double.MinValue), (p.x, Double.MaxValue))
    )

  "Two segments shaping a ┼" must "intersect but not be joined" in {
    check(forAll(pSegments) { case (s1, s2) => s1.isNotJoiningButIntersecting(s2) })
  }

  val xSegments: Gen[(Segment2D, Segment2D)] = for {
    s <- genSegment2D
  } yield
    (
      Segment2D.fromCoords2D((Double.MinValue, s.s.y), (Double.MaxValue, s.e.y)),
      Segment2D.fromCoords2D((s.s.x, Double.MinValue), (s.e.x, Double.MaxValue))
    )

  "Two segments shaping a ╳" must "intersect but not be joined" in {
    check(forAll(xSegments) { case (s1, s2) => s1.isNotJoiningButIntersecting(s2) })
  }

}
