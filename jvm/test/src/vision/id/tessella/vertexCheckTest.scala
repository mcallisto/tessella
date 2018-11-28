package vision.id.tessella

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

import vision.id.tessella.Polar.UnitRegularPgon
import vision.id.tessella.Tau.τ

class vertexCheckTest extends FlatSpec with Checkers with MathUtils {

  "The vertex joining 2 regular p-gons" must "be less than the full circle" in {
    val twoSides: Gen[List[Int]] = Gen.listOfN(2, Gen.choose(3, 100))
    check(forAll(twoSides)(_.map(UnitRegularPgon.ofSides(_).α).sum < τ))
  }

  "The vertex joining 7 regular p-gons" must "be more than the full circle" in {
    val sevenSides: Gen[List[Int]] = Gen.listOfN(7, Gen.choose(3, 100))
    check(forAll(sevenSides)(_.map(UnitRegularPgon.ofSides(_).α).sum > τ))
  }

}
