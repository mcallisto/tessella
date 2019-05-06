package vision.id.tessella

import java.lang.Math.{abs, pow}

trait MathUtils extends ListUtils {

  val decimalPrecision: Int = 8

  val stdPrecision: Double = pow(10.0, -decimalPrecision)

  final implicit class Doub(d: Double) {

    /**
      * compare double at a given precision
      * see https://alvinalexander.com/scala/how-to-compare-floating-point-numbers-in-scala-float-double
      *
      * @param other     other number to be compared
      * @param precision level of precision
      * @return
      */
    def ~=(other: Double, precision: Double = stdPrecision): Boolean =
      (d - other).abs < precision

    def <~=(other: Double, precision: Double = stdPrecision): Boolean =
      d < other || (d ~= (other, precision))

    def >~=(other: Double, precision: Double = stdPrecision): Boolean =
      d > other || (d ~= (other, precision))

    def <<(other: Double, precision: Double = stdPrecision): Boolean =
      ! >~=(other, precision)

    def >>(other: Double, precision: Double = stdPrecision): Boolean =
      ! <~=(other, precision)

    def isInSortedRange(low: Double, high: Double): Boolean =
      (d >> low) && (d << high)

    def isInRange(end1: Double, end2: Double): Boolean =
      List(end1, end2).sorted.onlyTwoElements(isInSortedRange)

    def roundAt(precision: Int = decimalPrecision): Double =
      BigDecimal(d).setScale(precision, BigDecimal.RoundingMode.HALF_UP).toDouble

    def toRoundedDegrees(precision: Int = 0): Double = d.toDegrees.roundAt(precision)

  }

  /**
    * this method differs from % in that it always returns a non-negative
    *
    * @param a dividend
    * @param d divisor
    * @return
    */
  def mod(a: Double, d: Double): Double = a % d match {
    case neg if neg < 0 => neg + abs(d)
    case nonNeg         => nonNeg
  }

  def mod(a: Int, d: Int): Int = a % d match {
    case neg if neg < 0 => neg + abs(d)
    case nonNeg         => nonNeg
  }

}
