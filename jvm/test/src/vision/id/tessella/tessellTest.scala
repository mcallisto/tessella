package vision.id.tessella

import org.scalatest.FlatSpec
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import vision.id.tessella.Alias.Tiling

class tessellTest extends FlatSpec with Methods {

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

  val minCaseStudy: Tiling = Tiling.poly(4) ++ List(3 ~ 5, 5 ~ 4)

  "Two triangles around a vertex" can "be a tessellation and have a perimeter" in {
    assert(minCaseStudy.periEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 5, 5 ~ 4, 4 ~ 1))
  }

  they can "have the perimeter nodes ordered" in {
    //assert(minCaseStudy.orderedNodes.init === List(1, 2, 3, 5, 4))
    assert(minCaseStudy.periNodes.init === List(1, 2, 3, 5, 4))
  }

  "A tessellation" must "have a perimeter" in {
    assert(
      Tiling
        .hexagonNet(2, 1)
        .periEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 5, 5 ~ 10, 10 ~ 9, 9 ~ 8, 8 ~ 7, 7 ~ 6, 6 ~ 1))
    assert(Tiling.squareNet(3, 1).periEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 8, 8 ~ 7, 7 ~ 6, 6 ~ 5, 5 ~ 1))
    assert(Tiling.triangleNet(4, 1).periEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 6, 6 ~ 5, 5 ~ 4, 4 ~ 1))
  }

  "Several difficult perimeter cases" can "be identified" in {
    val case1: Tiling =
      Tiling.poly(4) ++ List(
        2 ~ 5,
        5 ~ 6,
        6 ~ 3,
        5 ~ 7,
        7 ~ 8,
        8 ~ 6,
        7 ~ 9,
        9 ~ 10,
        10 ~ 8
      )
    assert(case1.periEdges === List(1 ~ 2, 2 ~ 5, 5 ~ 7, 7 ~ 9, 9 ~ 10, 10 ~ 8, 8 ~ 6, 6 ~ 3, 3 ~ 4, 4 ~ 1))
    val case2: Tiling = Tiling.poly(3) ++ List(2 ~ 4, 3 ~ 4, 5 ~ 4, 5 ~ 2, 4 ~ 6, 5 ~ 6)
    assert(case2.periEdges === List(1 ~ 2, 2 ~ 5, 5 ~ 6, 6 ~ 4, 4 ~ 3, 3 ~ 1))
    val case3: Tiling = case1 ++ List(
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
    )
    assert(
      case3.periEdges === List(
        1 ~ 2,
        2 ~ 11,
        11 ~ 13,
        13 ~ 14,
        14 ~ 12,
        12 ~ 5,
        5 ~ 7,
        7 ~ 9,
        9 ~ 10,
        10 ~ 8,
        8 ~ 6,
        6 ~ 16,
        16 ~ 18,
        18 ~ 17,
        17 ~ 15,
        15 ~ 3,
        3 ~ 4,
        4 ~ 1
      ))
    val case4 = Tiling.fromG(Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 1, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1))
    assert(case4.periEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 5, 5 ~ 6, 6 ~ 7, 7 ~ 1))
    val case5 = Tiling.triangleNet(6, 2)
    assert(case5.periEdges === List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 8, 8 ~ 12, 12 ~ 11, 11 ~ 10, 10 ~ 9, 9 ~ 5, 5 ~ 1))
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
