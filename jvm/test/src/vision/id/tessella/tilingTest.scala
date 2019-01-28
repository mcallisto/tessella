package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.GraphPredef._

import vision.id.tessella.Alias.Tiling

class tilingTest extends FlatSpec with TilingUtils {

//  val oneTriangle: Tessell = TessellGraph.poly(3)
//
//  "A regular triangle" can "be a tessellation" in {
//    assert(oneTriangle.graph === Graph(1 ~ 2, 2 ~ 3, 3 ~ 1))
//  }
//
//  val oneOctagon: Tessell = TessellGraph.poly(8)
//
//  "A regular octagon" can "be a tessellation" in {
//    assert(oneOctagon.graph === Graph(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 5, 5 ~ 6, 6 ~ 7, 7 ~ 8, 8 ~ 1))
//  }
//
//  val disconnected: Graph[Int, UnDiEdge] = oneTriangle.graph + 4 ~ 5
//
//  it must "be connected" in {
//    assertThrows[IllegalArgumentException](new Tessell(disconnected))
//  }
//
//  the[IllegalArgumentException] thrownBy new Tessell(disconnected) should have message
//    "requirement failed: tessell not connected"
//
//  val nodesWith7Edges: Graph[Int, UnDiEdge] = oneOctagon.graph + (1 ~ 3, 1 ~ 4, 1 ~ 5, 1 ~ 6, 1 ~ 7)
//
//  it must "have nodes each with 2 <= edges <= 6" in {
//    assertThrows[IllegalArgumentException](new Tessell(nodesWith7Edges))
//  }
//
//  the[IllegalArgumentException] thrownBy new Tessell(nodesWith7Edges) should have message
//    "requirement failed: nodes with wrong number of edges: Set(1)"

  val minCaseStudy: Tiling = Square.toTiling ++ Set(3 ~ 5, 5 ~ 4).map(Side.fromEdge(_))

  "One square and one triangle around a vertex" can "be a tessellation and have a perimeter" in {
    assert(minCaseStudy.perimeterOrderedEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 5, 5 ~ 4, 4 ~ 1))
    assert(minCaseStudy.edges.toString === "EdgeSet(1-2, 5-4, 2-3, 3-5, 3=4, 4-1)")
  }

  they can "have the perimeter nodes ordered" in {
    //assert(minCaseStudy.orderedNodes.init === List(1, 2, 3, 5, 4))
    assert(minCaseStudy.perimeterOrderedNodes.init === List(1, 2, 3, 5, 4))
  }

  "A tessellation" must "have a perimeter" in {
    assert(
      Tiling
        .hexagonNet(2, 1)
        .perimeterOrderedEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 5, 5 ~ 10, 9 ~ 10, 8 ~ 9, 7 ~ 8, 6 ~ 7, 1 ~ 6))
    assert(
      Tiling
        .squareNet(3, 1)
        .perimeterOrderedEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 8, 7 ~ 8, 6 ~ 7, 5 ~ 6, 1 ~ 5))
    assert(
      Tiling
        .triangleNet(4, 1)
        .perimeterOrderedEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 6, 5 ~ 6, 4 ~ 5, 1 ~ 4))
  }

  "Several difficult perimeter cases" can "be identified" in {
    val case1: Tiling =
      Square.toTiling ++ Set(
        2 ~ 5,
        5 ~ 6,
        6 ~ 3,
        5 ~ 7,
        7 ~ 8,
        8 ~ 6,
        7 ~ 9,
        9 ~ 10,
        10 ~ 8
      ).map(Side.fromEdge(_))
    assert(case1.perimeterOrderedEdges === List(1 ~ 2, 2 ~ 5, 5 ~ 7, 7 ~ 9, 9 ~ 10, 10 ~ 8, 8 ~ 6, 6 ~ 3, 3 ~ 4, 4 ~ 1))
    val case2: Tiling = Triangle.toTiling ++ Set(2 ~ 4, 3 ~ 4, 5 ~ 4, 5 ~ 2, 4 ~ 6, 5 ~ 6).map(Side.fromEdge(_))
    assert(case2.perimeterOrderedEdges === List(1 ~ 2, 5 ~ 2, 5 ~ 6, 4 ~ 6, 3 ~ 4, 3 ~ 1))
    val case3: Tiling = case1 ++ Set(
      2 ~ 11,
      11 ~ 12,
      12 ~ 5,
      13 ~ 11,
      13 ~ 14,
      14 ~ 12,
      3 ~ 15,
      15 ~ 16,
      16 ~ 6,
      15 ~ 17,
      17 ~ 18,
      18 ~ 16
    ).map(Side.fromEdge(_))
    assert(
      case3.perimeterOrderedEdges === List(1 ~ 2,
                                           2 ~ 11,
                                           13 ~ 11,
                                           13 ~ 14,
                                           14 ~ 12,
                                           12 ~ 5,
                                           5 ~ 7,
                                           7 ~ 9,
                                           9 ~ 10,
                                           10 ~ 8,
                                           8 ~ 6,
                                           16 ~ 6,
                                           18 ~ 16,
                                           17 ~ 18,
                                           15 ~ 17,
                                           3 ~ 15,
                                           3 ~ 4,
                                           4 ~ 1))
    val case4 = Tiling.fromSides(
      Set(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 1, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1).map(Side.fromEdge(_)))
    assert(case4.perimeterOrderedEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 5, 5 ~ 6, 6 ~ 7, 7 ~ 1))
    val case5 = Tiling.triangleNet(6, 2)
    assert(
      case5.perimeterOrderedEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 8, 8 ~ 12, 11 ~ 12, 10 ~ 11, 9 ~ 10, 5 ~ 9, 1 ~ 5))
  }

  val three: Tiling = Tiling.threeUniformOneOneOne8(6, 6)

  "A tessellation" can "have its pgon counted by type" in {
    assert(
      three.pgonsMap === Map(
        4 -> 18,
        3 -> 38,
        6 -> 12
      ))
  }

  it must "have a gonality" in {
    assert(three.gonality === 3)
  }

}
