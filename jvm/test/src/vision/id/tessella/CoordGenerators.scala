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
      p1 <- genPoint2D suchThat (_ != p0)
    } yield new Segment2D(p0, p1)

  def genBillionPoint2D: Gen[Point2D] =
    for {
      x <- Gen.choose(-oneBillion, oneBillion)
      y <- Gen.choose(-oneBillion, oneBillion)
    } yield new Point2D(x, y)

  def genOrderedDiagonal: Gen[Segment2D] =
    for {
      p0 <- genBillionPoint2D
      x <- Gen.choose(p0.x + stdPrecision, oneBillion * 2)
      y <- Gen.choose(p0.y + stdPrecision, oneBillion * 2)
    } yield new Segment2D(p0, new Point2D(x, y))

}
