package vision.id.tessella

import java.lang.Math.pow

import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import vision.id.tessella.Tau.TAU

class mathCheckTest extends FlatSpec with Checkers with CoordGenerators {

  "The non negative modulo method for Double" must "always return non negative results" in {
    check(forAll { (dividend: Double, divisor: Double) =>
      mod(dividend, divisor) >= 0.0
    })
  }

  it must "always return results smaller in absolute than the divisor" in {
    check(forAll { (dividend: Double, divisor: Double) =>
      val result = mod(dividend, divisor)
      (dividend, divisor) match {
        case (_, 0.0)                                                      => result.isNaN
        case (dend, _) if dend.abs > oneBillion || dend.abs < stdPrecision => result <= divisor.abs
        case _                                                             => result < divisor.abs
      }
    })
  }

  it must "always return results smaller in absolute when the divisor is TAU" in {
    check(forAll(arbitrary[Double]) {
      case small if small.abs < stdPrecision => mod(small, TAU) <= TAU
      case dividend                          => mod(dividend, TAU) < TAU
    })
  }

  "The standard modulo method for Double" can "return also negative results" in {
    check(forAll { (dividend: Double, divisor: Double) =>
      dividend % divisor >= 0.0 || dividend % divisor <= 0.0
    })
  }

  it must "always return results smaller in absolute than the divisor" in {
    check(forAll { (dividend: Double, divisor: Double) =>
      val result = dividend % divisor
      (dividend, divisor) match {
        case (_, 0.0)                                                      => result.isNaN
        case (dend, _) if dend.abs > oneBillion || dend.abs < stdPrecision => result <= divisor.abs
        case _                                                             => result < divisor.abs
      }
    })
  }

  val nonZeroInt: Gen[Int] = arbitrary[Int] suchThat (_ != 0)

  "The non negative modulo method for Int" must "always return non negative results" in {
    check(forAll(arbitrary[Int], nonZeroInt) { (dividend, divisor) =>
      mod(dividend, divisor) >= 0
    })
  }

  it must "always return results smaller in absolute than the divisor" in {
    check(forAll(arbitrary[Int], nonZeroInt) {
      case (_, Int.MinValue)   => true
      case (dividend, divisor) => mod(dividend, divisor) < divisor.abs
    })
  }

  "The standard modulo method for Int" can "return also negative results" in {
    check(forAll(arbitrary[Int], nonZeroInt) { (dividend, divisor) =>
      dividend % divisor >= 0 || dividend % divisor <= 0
    })
  }

  it must "always return results smaller in absolute than the divisor" in {
    check(forAll(arbitrary[Int], nonZeroInt) {
      case (_, Int.MinValue)   => true
      case (dividend, divisor) => (dividend % divisor).abs < divisor.abs
    })
  }

  "~= (approximately equal)" must "be true if precision is lower" in {
    assert(0.0 ~= (0.001, 0.01))
  }

  it must "be false if precision is equal or higher" in {
    assert(!(0.0 ~= (0.001, 0.001)))
    assert(!(0.0 ~= (0.001, 0.0001)))
  }

  it must "be always true within given precision" in {
    val generators: Gen[(Double, Double)] = for {
      decimalPrecision <- Gen.choose(0, 10)
      precision: Double = pow(10.0, -decimalPrecision)
      aroundZero <- Gen.choose(-precision, precision) // precision value not included
    } yield (aroundZero, precision)

    check(forAll(generators) {
      case (aroundZero, precision) => (0.0 ~= (aroundZero, precision)) && !(0.0 ~= (precision, precision))
    })

  }

}
