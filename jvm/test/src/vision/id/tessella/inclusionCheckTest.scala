package vision.id.tessella

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import vision.id.tessella.Cartesian2D.{OrderedUnitSegment2D, Point2D, Polygon, Segment2D}
import vision.id.tessella.Polar.PointPolar
import vision.id.tessella.Tau.TAU

class inclusionCheckTest extends FlatSpec with Checkers with CoordGenerators with TryUtils with Grid {

  "A point with positive x" must "be to the right of the y ax" in {
    val segment = Segment2D.fromCoords2D((0.0, 0.0), (0.0, 1.0))

    check(forAll(genPoint2D) { point =>
      segment.isLeft(point) match {
        case left if left > 0   => point.x < 0.0
        case right if right < 0 => point.x > 0.0
        case _                  => point.x == 0.0
      }
    })
  }

  "A point with negative y" must "be to the right of the x ax" in {
    val segment = Segment2D.fromCoords2D((0.0, 0.0), (1.0, 0.0))

    check(forAll(genPoint2D) { point =>
      segment.isLeft(point) match {
        case left if left > 0   => point.y > 0.0
        case right if right < 0 => point.y < 0.0
        case _                  => point.y == 0.0
      }
    })
  }

  "A point with x > y" must "be to the right of the x = y diagonal" in {
    val segment = Segment2D.fromCoords2D((0.0, 0.0), (1.0, 1.0))

    check(forAll(genPoint2D) { point =>
      segment.isLeft(point) match {
        case left if left > 0   => point.x < point.y
        case right if right < 0 => point.x > point.y
        case _                  => point.x == point.y
      }
    })
  }

  "A point with x > -y" must "be to the right of the other x = -y diagonal" in {
    val segment = Segment2D.fromCoords2D((0.0, 0.0), (-1.0, 1.0))

    check(forAll(genPoint2D) { point =>
      segment.isLeft(point) match {
        case left if left > 0   => point.x < -point.y
        case right if right < 0 => point.x > -point.y
        case _                  => point.x == -point.y
      }
    })
  }

  "The winding number of a point relative to a segment" can "be calculated" in {
    val components: Gen[(Point2D, Segment2D)] = for {
      point   <- genPoint2D
      segment <- genSegment2D
    } yield (point, segment)

    check(forAll(components) {
      case (point, segment) =>
        segment.windingNumber(point) match {
          case 1  => point.y >= segment.s.y && point.y < segment.e.y && segment.isLeft(point) > 0
          case -1 => point.y < segment.s.y && point.y >= segment.e.y && segment.isLeft(point) < 0
          case 0  => true
        }
    })
  }

  "The winding number of both endpoints of a segment" must "be 0" in {
    check(forAll(genSegment2D) { segment =>
      segment.windingNumber(segment.s) == 0 && segment.windingNumber(segment.e) == 0
    })
  }

  def createRectanglePgon(diagonal: Segment2D): Polygon =
    new Polygon(
      List(
        new Point2D(diagonal.s.x, diagonal.s.y),
        new Point2D(diagonal.e.x, diagonal.s.y),
        new Point2D(diagonal.e.x, diagonal.e.y),
        new Point2D(diagonal.s.x, diagonal.e.y)
      ))

  "A point" can "be checked for inclusion in a rectangle" in {
    val components: Gen[(Point2D, Segment2D)] = for {
      point    <- genPoint2D
      diagonal <- genOrderedDiagonal
    } yield (point, diagonal)

    check(forAll(components) {
      case (point, diagonal) =>
        createRectanglePgon(diagonal).includes(point) === diagonal.includesInBox(point)
    })
  }

  // @todo need to understand why
  "Only the bottom left vertex" must "be included in its rectangle" in {
    check(forAll(genOrderedDiagonal) { diagonal =>
      val pgon = createRectanglePgon(diagonal)
      pgon.points match {
        case h :: t => pgon.includes(h) && pgon.toSegments2D(1).windingNumber(h) == 1 && t.forall(!pgon.includes(_))
        case Nil    => throw new Error
      }
    })
  }

  "A segment" can "be checked for inclusion in a rectangle" in {
    val components: Gen[(Segment2D, Segment2D)] = for {
      segment  <- genSegment2D
      diagonal <- genOrderedDiagonal
    } yield (segment, diagonal)

    check(forAll(components) {
      case (segment, diagonal) =>
        createRectanglePgon(diagonal).includes(segment) ===
          (diagonal.includesInBox(segment.s) && diagonal.includesInBox(segment.e))
    })
  }

  "A unit segment" can "be checked for inclusion in a rectangle" in {
    val components: Gen[(OrderedUnitSegment2D, Segment2D)] = for {
      pointX    <- Gen.choose(-1.0, 1.0)
      pointY    <- Gen.choose(-1.0, 1.0)
      topRightX <- Gen.choose(0.0, 1.0)
      topRightY <- Gen.choose(0.0, 1.0)
      angle <- Gen.choose(0.0, TAU)
      segment = Segment2D.fromCoords2D((topRightX, topRightY), (topRightX - 1.0, topRightY - 1.0))
      unit = OrderedUnitSegment2D.fromSegment2D(new PointPolar(1.0, angle).toSegment2D.sum(new Point2D(pointX, pointY)))
    } yield (unit, segment)

    check(forAll(components) {
      case (unit, diagonal) =>
        createRectanglePgon(diagonal).includes(unit) ===
          (diagonal.includesInBox(unit.s) && diagonal.includesInBox(unit.e))
    })

  }

  "Each side" must "NOT be included in its rectangle" in {
    check(forAll(genOrderedDiagonal) { diagonal =>
      val pgon  = createRectanglePgon(diagonal)
      val sides = pgon.toSegments2D
      sides.forall(!pgon.includes(_))
    })
  }

  "Each segment attached to sides" must "NOT be included in its rectangle" in {
    val components: Gen[(Segment2D, Double, Double, Double, Double)] = for {
      diagonal <- genOrderedDiagonal
      x1       <- Gen.choose(diagonal.s.x, diagonal.e.x)
      x2       <- Gen.choose(diagonal.s.x, diagonal.e.x)
      y1       <- Gen.choose(diagonal.s.y, diagonal.e.y)
      y2       <- Gen.choose(diagonal.s.y, diagonal.e.y)
    } yield (diagonal, x1, x2, y1, y2)

    check(forAll(components) {
      case (diagonal, x1, x2, y1, y2) =>
        val pgon   = createRectanglePgon(diagonal)
        val center = pgon.barycenter
        val segments = List(
          Segment2D.fromCoords2D((x1, diagonal.s.y), (x1, diagonal.e.y)), // parallel
          Segment2D.fromCoords2D((x1, diagonal.s.y), (x2, diagonal.e.y)), // not parallel
          Segment2D.fromCoords2D((diagonal.s.x, y1), (diagonal.e.x, y1)), // parallel
          Segment2D.fromCoords2D((diagonal.s.x, y1), (diagonal.e.x, y2)), // not parallel
          Segment2D.fromCoords2D((x1, diagonal.s.y), (center.x, center.y)), // attached to left
          Segment2D.fromCoords2D((x2, diagonal.e.y), (center.x, center.y)), // attached to right
          Segment2D.fromCoords2D((diagonal.s.x, y1), (center.x, center.y)), // attached to top
          Segment2D.fromCoords2D((diagonal.e.x, y2), (center.x, center.y)) // attached to bottom
        )
        segments.forall(!pgon.includes(_))
    })
  }

}
