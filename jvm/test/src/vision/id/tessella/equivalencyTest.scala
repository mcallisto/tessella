package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._

import vision.id.tessella.Others.Mono
import vision.id.tessella.Tessella.Tiling

class equivalencyTest extends FlatSpec with ConstraintUtils with Loggable {

  setLogLevel("WARN")

  val squareNet4: Mono = Mono.squareNet(4)

  "A 4x4 square net" must "be perimeter equivalent and equivalent to itself" in {
    assert(squareNet4.isPolygonSymmetricTo(squareNet4) === true)
    assert(squareNet4.isEquivalentTo(squareNet4) === true)
  }

  val squareNet6x2: Tiling = Tiling.squareNet(6, 2)

  it must "have the same perimeter length of a 6x2 square" in {
    assert(squareNet4.toPerimeterSimplePolygon.safeGet.perimeter === squareNet6x2.toPerimeterSimplePolygon.safeGet.perimeter)
  }

  it must "be NOT perimeter equivalent and NOT equivalent to a 6x2 square" in {
    assert(squareNet4.isPolygonSymmetricTo(squareNet6x2) === false)
    assert(squareNet4.isEquivalentTo(squareNet6x2) === false)
  }

  it must "be perimeter equivalent and equivalent to another 4x4 square net with a different node order" in {
    val anothersquareNet4 = Tiling.squareNet(4, 4)
    assert(squareNet4.isPolygonSymmetricTo(anothersquareNet4) === true)
    assert(squareNet4.isEquivalentTo(anothersquareNet4) === true)
  }

  "An 5x5 hexagon" must "be perimeter equivalent but NOT equivalent to the same with some hexs substituted by 3*6" in {
    val hexNet5 = Tiling.hexagonNet(5, 5)
    val sameWithDifferentGrid = Tiling.threeUniformTwoOne(5, 5)
    assert(hexNet5.isPolygonSymmetricTo(sameWithDifferentGrid) === true)
    assert(hexNet5.isEquivalentTo(sameWithDifferentGrid) === false)
  }

//  "A hex of side composed by ▲⁶" must "be perimeter equivalent but NOT equivalent to the same size hex by ⬣.▲.⬣.▲" in {
//    val triHex3 = Mono.triangularHex(3)
//    val sameWithDifferentGrid = Mono.triHexagonalHexoid(2)
//    assert(triHex3.isPolygonSymmetricTo(sameWithDifferentGrid) === true)
//    assert(triHex3.isEquivalentTo(sameWithDifferentGrid) === false)
//  }

//  val squareNet2: Tessell = TessellGraph.squareNet(2)
//  import squareNet2.PeriEdge
//
//  val ⦨ : Tessell = squareNet2.perimeter.get(UnDiEdge(2, 3)).addPgonOfSides(4).get
//
//  "A tessellation ⦨" must "be perimeter equivalent and equivalent to itself" in {
//    assert(⦨.isPolygonSymmetricTo(⦨) === true)
//    assert(⦨.isEquivalentTo(⦨) === true)
//  }
//
//  it must "be perimeter equivalent and equivalent to another ⦬ with different node order" in {
//    val ⦬ = squareNet2.perimeter.get(UnDiEdge(7, 8)).addPgonOfSides(4).get
//    assert(⦨.isPolygonSymmetricTo(⦬) === true)
//    assert(⦨.isEquivalentTo(⦬) === true)
//  }
//
//  it must "be perimeter equivalent and equivalent to another ⦭ with different node order" in {
//    val ⦭ = squareNet2.perimeter.get(UnDiEdge(8, 9)).addPgonOfSides(4).get
//    assert(⦨.isPolygonSymmetricTo(⦭) === true)
//    assert(⦨.isEquivalentTo(⦭) === true)
//  }
//
//  val triHex2: Tessell = TessellGraph.triangularHex(2)
//
//  val asymmHex: Tessell = {
//    import triHex2.PeriEdge
//    triHex2.perimeter.get(UnDiEdge(9, 19)).addPgonOfSides(4).get
//  }
//
//  "An asymmetric hex" must "be perimeter equivalent and equivalent to itself" in {
//    assert(asymmHex.isPolygonSymmetricTo(asymmHex) === true)
//    assert(asymmHex.isEquivalentTo(asymmHex) === true)
//  }
//
//  it must "be perimeter equivalent and equivalent to another ('reflected') one with different node order" in {
//    val asymmHexRefl = {
//      import triHex2.PeriEdge
//      triHex2.perimeter.get(UnDiEdge(9, 10)).addPgonOfSides(4).get
//    }
//    assert(asymmHex.isPolygonSymmetricTo(asymmHexRefl) === true)
//    assert(asymmHex.isEquivalentTo(asymmHexRefl) === true)
//  }
//
//  it must "be perimeter equivalent and equivalent to another ('rotated') one with different node order" in {
//    val asymmHexRot = {
//      import triHex2.PeriEdge
//      triHex2.perimeter.get(UnDiEdge(8, 10)).addPgonOfSides(4).get
//    }
//    assert(asymmHex.isPolygonSymmetricTo(asymmHexRot) === true)
//    assert(asymmHex.isEquivalentTo(asymmHexRot) === true)
//  }
//
//  "A list of equivalent tessell" can "be distinctively filtered" in {
//    assert(TessellGraph.equivalentDistinct(List.fill(4)(squareNet4)) === List(squareNet4))
//  }
//
//  "Two given tessellations" can "be equivalent" in {
//    val a = new Tessell(Graph(
//        1 ~ 2, 2 ~ 3, 2 ~ 18, 3 ~ 1, 3 ~ 4, 3 ~ 10, 4 ~ 1, 4 ~ 5, 5 ~ 1, 5 ~ 6, 6 ~ 1, 6 ~ 7, 6 ~ 19, 7 ~ 8, 8 ~ 9,
//        9 ~ 2, 10 ~ 11, 10 ~ 18, 10 ~ 20, 11 ~ 12, 12 ~ 13, 12 ~ 17, 13 ~ 4, 13 ~ 14, 14 ~ 15, 14 ~ 22, 14 ~ 27,
//        15 ~ 16, 15 ~ 27, 15 ~ 29, 16 ~ 5, 17 ~ 13, 17 ~ 14, 17 ~ 21, 18 ~ 3, 19 ~ 5, 19 ~ 16, 19 ~ 30, 20 ~ 18,
//        21 ~ 12, 21 ~ 26, 22 ~ 17, 22 ~ 23, 23 ~ 24, 24 ~ 25, 25 ~ 21, 26 ~ 11, 26 ~ 12, 26 ~ 28, 27 ~ 22, 28 ~ 11,
//        29 ~ 27, 30 ~ 16))
//    val b = new Tessell(Graph(
//        1 ~ 2, 2 ~ 3, 2 ~ 15, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 1, 5 ~ 6, 5 ~ 10, 6 ~ 1, 6 ~ 7, 7 ~ 8, 7 ~ 14, 8 ~ 9,
//        8 ~ 21, 8 ~ 27, 9 ~ 2, 9 ~ 27, 9 ~ 29, 10 ~ 6, 10 ~ 11, 10 ~ 16, 10 ~ 20, 11 ~ 12, 11 ~ 20, 11 ~ 22, 12 ~ 13,
//        13 ~ 7, 13 ~ 14, 14 ~ 8, 14 ~ 23, 15 ~ 3, 16 ~ 5, 16 ~ 17, 17 ~ 18, 18 ~ 19, 19 ~ 4, 20 ~ 16, 21 ~ 14, 21 ~ 24,
//        22 ~ 20, 23 ~ 13, 23 ~ 28, 24 ~ 25, 25 ~ 26, 26 ~ 23, 27 ~ 21, 28 ~ 12, 28 ~ 13, 28 ~ 30, 29 ~ 27, 30 ~ 12))
//    assert(a.isEquivalentTo(b) === true)
//  }
//
//  "Other two given tessellations" must "be equivalent" in {
//    val a = new Tessell(Graph(
//      1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 3 ~ 21, 4 ~ 1, 4 ~ 5, 4 ~ 14, 5 ~ 1, 5 ~ 6, 5 ~ 10, 6 ~ 1, 6 ~ 7, 7 ~ 8, 7 ~ 16,
//      8 ~ 9, 8 ~ 17, 9 ~ 2, 9 ~ 15, 10 ~ 11, 10 ~ 18, 11 ~ 12, 11 ~ 19, 12 ~ 13, 12 ~ 20, 13 ~ 6, 13 ~ 7, 14 ~ 5,
//      14 ~ 10, 15 ~ 2, 16 ~ 8, 17 ~ 9, 18 ~ 11, 19 ~ 12, 20 ~ 13, 21 ~ 22, 22 ~ 23, 23 ~ 24, 24 ~ 4))
//    val b = new Tessell(Graph(
//      1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 4 ~ 14, 4 ~ 21, 5 ~ 1, 5 ~ 6, 5 ~ 10, 6 ~ 1, 6 ~ 7, 7 ~ 8, 7 ~ 16,
//      8 ~ 9, 8 ~ 17, 9 ~ 2, 9 ~ 15, 10 ~ 11, 10 ~ 18, 11 ~ 12, 11 ~ 19, 12 ~ 13, 12 ~ 20, 13 ~ 6, 13 ~ 7, 14 ~ 5,
//      14 ~ 10, 15 ~ 2, 16 ~ 8, 17 ~ 9, 18 ~ 11, 19 ~ 12, 20 ~ 13, 21 ~ 22, 22 ~ 23, 23 ~ 24, 24 ~ 14))
////    val (m1, m2) = (Full.s("(▲⁴.⬣)"), Full.s("(▲.⬣.▲.⬣)"))
////    val t = TessellGraph.expandTwoPatternsUntilMandatory(m1, m2)._1
////    import t.PeriEdge
////    val a1: Tessell = t.perimeter.get(3 ~ 4).addPgonOfSides(6).get
////    val b1: Tessell = t.perimeter.get(4 ~ 14).addPgonOfSides(6).get
////    assert(a.graph === a1.graph)
////    assert(b.graph === b1.graph)
//
//    assert(a.isPolygonSymmetricTo(b))
//    assert(a.isEquivalentTo(b) === true)
//  }
}