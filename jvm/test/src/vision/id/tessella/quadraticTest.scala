package vision.id.tessella

import org.scalatest.FlatSpec

import vision.id.tessella.Others.Mono

class quadraticTest extends FlatSpec {

  "A sequence" can "represent the regular growth of a triangularHex" in {
    val terms: Mono.terms = (6, 0, 0)
    assert((0 to 5).map(s => Mono.quadratic(terms)(s.toDouble).toInt) === List(0, 6, 24, 54, 96, 150))
  }

  it can "represent the regular growth of a squareNet" in {
    val terms: Mono.terms = (1, 0, 0)
    assert((0 to 5).map(s => Mono.quadratic(terms)(s.toDouble).toInt) === List(0, 1, 4, 9, 16, 25))
  }

  "A sequence of triangular numbers" can "represent the regular growth of an hexagonalHexoid" in {
    val terms = Mono.triangularToPolynomial(6, 1)
    assert((0 to 5).map(s => Mono.quadratic(terms)(s.toDouble).toInt) === List(1, 7, 19, 37, 61, 91))
  }

  "A sequence" can "represent the regular growth of a triSquareSquaroid" in {
    val terms: Mono.terms = (6, 12, 2)
    assert((0 to 5).map(s => Mono.quadratic(terms)(s.toDouble).toInt) === List(2, 20, 50, 92, 146, 212))
  }

  it can "represent the regular growth of a triSquareHexagonalHexoid" in {
    val terms: Mono.terms = (18, 12, 1)
    assert((0 to 5).map(s => Mono.quadratic(terms)(s.toDouble).toInt) === List(1, 31, 97, 199, 337, 511))
  }

  it can "represent the regular growth of both a triHexagonalHexoid and a triDodecagonalHexoid" in {
    val terms: Mono.terms = (9, 3, 1)
    assert((0 to 5).map(s => Mono.quadratic(terms)(s.toDouble).toInt) === List(1, 13, 43, 91, 157, 241))
  }

  it can "represent the regular growth of a squareHexDodecagonalHexoid" in {
    val terms: Mono.terms = (18, 10, -3)
    assert((0 to 5).map(s => Mono.quadratic(terms)(s.toDouble).toInt) === List(-3, 25, 89, 189, 325, 497))
  }

  it can "represent the regular growth of a squareOctogonalSquaroid" in {
    val terms: Mono.terms = (4, 12, 5)
    assert((0 to 5).map(s => Mono.quadratic(terms)(s.toDouble).toInt) === List(5, 21, 45, 77, 117, 165))
  }

}