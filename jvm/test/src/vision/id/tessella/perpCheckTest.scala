package vision.id.tessella

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec

import vision.id.tessella.Cartesian2D.Point2D

class perpCheckTest extends FlatSpec with Checkers with MathUtils with CoordGenerators {

  "The Point2D perp product" must "be nilpotent" in {
    val max = Math.sqrt(Double.MaxValue)
    check(forAll(genPoint2D suchThat (p ⇒ p.x.abs < max && p.y.abs < max)) { p ⇒
      p.perp(p) == 0.0
    })
  }

  val Point2DPairGen: Gen[(Point2D, Point2D)] = for {
    p0 ← genPoint2D
    p1 ← genPoint2D
  } yield (p0, p1)

  it must "be antisymmetric" in {
    check(forAll(Point2DPairGen) { case (p0, p1) ⇒ p0.perp(p1) == -p1.perp(p0) })
  }

}
