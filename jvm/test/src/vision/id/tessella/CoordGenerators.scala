package vision.id.tessella

import java.lang.Math.pow

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

import vision.id.tessella.Cartesian2D.{Point2D, Segment2D}

trait CoordGenerators extends MathUtils {

  val oneBillion: Double = pow(10.0, 9)

  val oneMillion: Double = pow(10.0, 6)

  def withinPrecision: Gen[Double] = arbitrary[Double] suchThat (_.abs < stdPrecision)

  def genPoint2D: Gen[Point2D] =
    for {
      x <- arbitrary[Double]
      y <- arbitrary[Double]
    } yield new Point2D(x, y)

  def genSegment2D: Gen[Segment2D] =
    for {
      p0 <- genPoint2D
      p1 <- genPoint2D suchThat (!_.equals(p0))
    } yield Segment2D.fromPoint2Ds(p0, p1)

}
