package vision.id.tessella.creation

import vision.id.tessella.Full
import vision.id.tessella.Others.Mono

/**
  * slow methods to create tessellations with growth governed by quadratic functions
  */
trait Quadratic extends Growth {

  /**
    * (a, b, c) elements of the polynomial ax^2+bx+c)
    */
  type terms = (Double, Double, Double)

  /**
    * function to calculate the polynomial of grade 2 with the given terms
    *
    * @param terms elements of the polynomial
    * @return
    */
  def quadratic(terms: terms): Double => Double = {
    val (a, b, c) = terms
    x =>
      a * x * x + b * x + c
  }

  /**
    * grow a pattern according to a polynomial
    *
    * @param side    length
    * @param pattern full vertex
    * @param terms   elements of the polynomial of grade 2 relating number of sides with number of p-gons
    * @return
    */
  private def growWithPolynomial(side: Int, pattern: Full, terms: terms): Mono =
    expandPattern(pattern, quadratic(terms)(side.toDouble).toInt).safeGet

  /**
    * grow a (3*6) hexagon of given side
    *
    * @note the size in p-gons is 6x^2 where x = side
    * @param side length in units
    * @return
    */
  def triangularHex(side: Int): Mono =
    growWithPolynomial(side, Full.s("(3*6)"), (6, 0, 0))

  /**
    * grow a (4*4) square of given side
    *
    * @note the size in p-gons is 4x^2 where x = side
    * @param side length in units
    * @return
    */
  def squareNet(side: Int): Mono =
    growWithPolynomial(side, Full.s("(4*4)"), (1, 0, 0))

  /**
    * get the polynomial of grade 2 representing a series of triangular numbers
    *
    * @param mult multiplier
    * @param add  addition
    * @return elements of the polynomial
    */
  def triangularToPolynomial(mult: Int = 1, add: Int = 0): terms =
    (0.5 * mult, 0.5 * mult, add)

  /**
    * grow a (6*3) hexoid of given side
    *
    * @note the size in p-gons is 3x^2+3x+1 where x = side
    * @param side length in hexagons (approx. √3 each)
    * @return
    */
  def hexagonalHexoid(side: Int): Mono =
    growWithPolynomial(side - 1, Full.s("(6*3)"), triangularToPolynomial(mult = 6, add = 1))

  /**
    * grow a (3*2.4.3.4) squaroid of given side
    *
    * @note the size in p-gons is 6x^2+12x+2 that is 6(x^2+1) where x = side
    * @param side length in squares (approx. ???)
    * @return
    */
  def triSquareSquaroid(side: Int): Mono =
    growWithPolynomial(side, Full.s("(3*2.4.3.4)"), (6, 12, 2))

  /**
    * grow a (3.4.6.4) hexoid of given side
    *
    * @note the size in p-gons is 18x^2+12x+1 where x = side
    * @param side length in hexagons (approx. 1+√3 each)
    * @return
    */
  def triSquareHexagonalHexoid(side: Int): Mono =
    growWithPolynomial(side - 1, Full.s("(3.4.6.4)"), (18, 12, 1))

  /**
    * grow a (3.6.3.6) hexagon of given side
    *
    * @note the size in p-gons is 9x^2+3x+1 where x = side
    * @param side length in hexagons (2x-1 units)
    * @return
    */
  def triHexagonalHexoid(side: Int): Mono =
    growWithPolynomial(side - 1, Full.s("(3.6.3.6)"), (9, 3, 1))

  /**
    * grow a (3.12*2) hexoid of given side
    *
    * @note the size in p-gons is 9x^2+3x+1 where x = side
    * @param side length in dodecagons (approx. ???)
    * @return
    */
  def triDodecagonalHexoid(side: Int): Mono =
    growWithPolynomial(side - 1, Full.s("(3.12*2)"), (9, 3, 1))

  /**
    * grow a (4.6.12) hexoid of given side
    *
    * @note the size in p-gons is 18x^2+10x-3 where x = side
    * @param side length in dodecagons (approx. ???)
    * @return
    */
  def squareHexDodecagonalHexoid(side: Int): Mono =
    growWithPolynomial(side, Full.s("(4.6.12)"), (18, 10, -3))

  /**
    * grow a (4.8*2) squaroid of given side
    *
    * @note the size in p-gons is 4x^2+12x+5 where x = side
    * @param side length in squares (approx. 2+2√2 each)
    * @return
    */
  def squareOctogonalSquaroid(side: Int): Mono =
    growWithPolynomial(side, Full.s("(4.8*2)"), (4, 12, 5))

}
