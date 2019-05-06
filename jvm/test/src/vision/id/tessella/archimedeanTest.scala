package vision.id.tessella

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

import vision.id.tessella.Tau.TAU
import vision.id.tessella.Polar.RegularPgon

class archimedeanTest extends FlatSpec with MathUtils with SVG with Checkers {

  def interiorAngle(sides: Int): Double = RegularPgon.angleFrom(sides)

  "The only monohedral tilings by regular p-gons" must "be (▲⁶), (■⁴) and (⬣³)" in {

    check(forAll(Gen.choose(3, 100))({
      case ok if List(3, 4, 6).contains(ok) =>
        val times = TAU / interiorAngle(ok)
        ((TAU % interiorAngle(ok)) ~= 0.0) && (ok match {
          case 3 => times ~= 6.0
          case 4 => times ~= 4.0
          case 6 => times ~= 3.0
        })
      case ko => !((TAU % interiorAngle(ko)) ~= 0.0)
    }))

  }

  def interiorAngleGen(min: Int, max: Int): Gen[Double] =
    for {
      sides <- Gen.choose(min, max)
    } yield interiorAngle(sides)

  val twoBigInteriors: Gen[List[Double]] = for {
    first  <- interiorAngleGen(100, 1000)
    second <- interiorAngleGen(100, 1000)
  } yield List(first, second)

  "A full vertex" must "be composed by at least 3 regular p-gons" in {
    check(forAll(twoBigInteriors)(TAU >> _.sum))
  }

  val sevenSmallInteriors: Gen[List[Double]] = for {
    first   <- interiorAngleGen(3, 6)
    second  <- interiorAngleGen(3, 6)
    third   <- interiorAngleGen(3, 6)
    fourth  <- interiorAngleGen(3, 6)
    fifth   <- interiorAngleGen(3, 6)
    sixth   <- interiorAngleGen(3, 6)
    seventh <- interiorAngleGen(3, 6)
  } yield List(first, second, third, fourth, fifth, sixth, seventh)

  it must "be composed by no more than 6 regular p-gons" in {
    check(forAll(sevenSmallInteriors)(TAU << _.sum))
  }

  it must "be composed by no more than 3 different regular p-gons" in {
    assert(List(3, 4, 5, 6).map(interiorAngle).sum >> TAU)
  }

}
