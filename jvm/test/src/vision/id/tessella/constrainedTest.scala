package vision.id.tessella

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import vision.id.tessella.Alias.Tiling

class constrainedTest extends FlatSpec {

  "A regular triangle" can "be a tessellation" in {
    val oneTriangle: Tiling = Tiling.poly(3)
    assert(oneTriangle.edges.toList === List(1 ~ 2, 2 ~ 3, 3 ~ 1))
  }

  val disconnected: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 4))

  "A disconnected graph" can "NOT be a tessellation" in {
    assert(disconnected.isConnected === false)
    assertThrows[IllegalArgumentException](Tiling.fromG(disconnected))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(disconnected) should have message
    "Addition refused: " +
      "nodes = GraphPredef(1, 5, 2, 6, 3, 4)" + ", " +
      "edges = GraphPredef(1~2, 5~6, 2~3, 6~4, 3~1, 4~5)"

  val nodesWith1Edge: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4))

  "A graph with 1-degree nodes" can "NOT be a tessellation" in {
    assert((nodesWith1Edge get 4).degree === 1)
    assertThrows[IllegalArgumentException](Tiling.fromG(nodesWith1Edge))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(nodesWith1Edge) should have message
    "Addition refused: " +
      "nodes = GraphPredef(1, 2, 3, 4)" + ", " +
      "edges = GraphPredef(1~2, 2~3, 3~1, 3~4)"

  val nodesWith7Edges: Graph[Int, UnDiEdge] = Graph.from(edges = List(2 ~ 3, 3 ~ 4, 4 ~ 5, 5 ~ 6, 6 ~ 7, 7 ~ 8, 8 ~ 2, 1 ~ 2, 1 ~ 3, 1 ~ 4, 1 ~ 5, 1 ~ 6, 1 ~ 7, 1 ~ 8))

  "A graph with a 7-degree node" can "NOT be a tessellation" in {
    assert((nodesWith7Edges get 1).degree === 7)
    assertThrows[IllegalArgumentException](Tiling.fromG(nodesWith7Edges))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(nodesWith7Edges) should have message
    "Addition refused: " +
      "nodes = GraphPredef(1, 5, 2, 6, 3, 7, 4, 8)" + ", " +
      "edges = GraphPredef(1~2, 1~3, 1~4, 1~5, 1~6, 1~7, 1~8, 5~6, 2~3, 6~7, 3~4, 7~8, 4~5, 8~2)"

  val negativeTriangle: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ -2, -2 ~ 3, 3 ~ 1))

  "A graph with nodes having negative value" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(negativeTriangle))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(negativeTriangle) should have message
    "Addition refused: " +
      "nodes = GraphPredef(1, -2, 3)" + ", " +
      "edges = GraphPredef(1~-2, -2~3, 3~1)"

  val papillon: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 1, 2 ~ 4, 4 ~ 5, 5 ~ 2))

  "A graph with two non adjacent p-gons sharing a vertex" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(papillon))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(papillon) should have message
    "Addition refused: " +
      "nodes = GraphPredef(1, 5, 2, 3, 4)" + ", " +
      "edges = GraphPredef(1~2, 5~2, 2~3, 2~4, 3~1, 4~5)"

  val threeAdjacentSquares: Graph[Int, UnDiEdge] =
    Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 1, 2 ~ 5, 5 ~ 6, 6 ~ 3, 4 ~ 7, 7 ~ 8, 8 ~ 3))

  val moreThanFullVertex: Graph[Int, UnDiEdge] = threeAdjacentSquares ++ List(8 ~ 9, 9 ~ 10, 10 ~ 6)
  "A graph with three squares and a pentagon at a vertex" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(moreThanFullVertex))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(moreThanFullVertex) should have message
    "Addition refused: nodes = GraphPredef(9, 1, 5, 2, 6, 3, 10, 7, 4, 8), edges = GraphPredef(9~10, 1~2, 5~6, 2~3, 2~5, 6~3, 3~4, 10~6, 7~8, 4~1, 4~7, 8~9, 8~3)"

  val areaOverlap1: Graph[Int, UnDiEdge] = threeAdjacentSquares ++ List(8 ~ 9, 9 ~ 10, 10 ~ 11, 11 ~ 3)
  "A graph with three squares and a regular pentagon overlapped at a vertex" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(areaOverlap1))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(areaOverlap1) should have message
    "Addition refused: " +
      "nodes = GraphPredef(9, 1, 5, 2, 6, 3, 10, 7, 4, 11, 8)" + ", " +
      "edges = GraphPredef(9~10, 1~2, 5~6, 2~3, 2~5, 6~3, 3~4, 10~11, 7~8, 4~1, 4~7, 11~3, 8~9, 8~3)"

  val sixSquares: Graph[Int, UnDiEdge] =
    threeAdjacentSquares ++ List(7 ~ 9, 9 ~ 10, 10 ~ 8, 10 ~ 11, 11 ~ 12, 12 ~ 8, 11 ~ 13, 13 ~ 14, 14 ~ 12)

  val vertexOverlapping: Graph[Int, UnDiEdge] = sixSquares ++ List(12 ~ 15, 15 ~ 16, 16 ~ 14)
  "A graph with an overlapping vertex" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(vertexOverlapping))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(vertexOverlapping) should have message
    "Addition refused: " +
      "nodes = GraphPredef(15, 9, 1, 16, 2, 3, 10, 4, 11, 12, 13, 5, 6, 7, 14, 8)" + ", " +
      "edges = GraphPredef(15~16, 9~10, 1~2, 16~14, 2~3, 2~5, 3~4, 10~11, 10~8, 4~1, 4~7, 11~13, 11~12, 12~15, 12~8, 13~14, 5~6, 6~3, 7~9, 7~8, 14~12, 8~3)"

  val areaOverlap2: Graph[Int, UnDiEdge] =
    sixSquares ++ List(12 ~ 15, 15 ~ 16, 16 ~ 17, 17 ~ 18, 18 ~ 19, 19 ~ 20, 20 ~ 14)
  "A graph with an overlapping area" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(areaOverlap2))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(areaOverlap2) should have message
    "Addition refused: " +
      "nodes = GraphPredef(15, 9, 1, 16, 2, 17, 3, 18, 10, 4, 11, 12, 19, 13, 5, 20, 6, 7, 14, 8)" + ", " +
      "edges = GraphPredef(15~16, 9~10, 1~2, 16~17, 2~3, 2~5, 17~18, 3~4, 18~19, 10~11, 10~8, 4~1, 4~7, 11~13, 11~12, 12~15, 12~8, 19~20, 13~14, 5~6, 20~14, 6~3, 7~9, 7~8, 14~12, 8~3)"

//  val gap: Graph[Int, UnDiEdge] = sixSquares ++ List(6 ~ 15,
//                                                     15 ~ 14,
//                                                     5 ~ 16,
//                                                     16 ~ 15,
//                                                     16 ~ 20,
//                                                     20 ~ 19,
//                                                     19 ~ 15,
//                                                     19 ~ 18,
//                                                     18 ~ 14,
//                                                     18 ~ 17,
//                                                     17 ~ 13)
//  "A graph with a gap" can "NOT be a tessellation" in {
//    assertThrows[IllegalArgumentException](Tiling.fromG(gap))
//  }
//
//  the[IllegalArgumentException] thrownBy Tiling.fromG(gap) should have message
//    "Addition refused: " +
//      "nodes = GraphPredef()" + ", " +
//      "edges = GraphPredef()"

  val preOverlap: Graph[Int, UnDiEdge] = threeAdjacentSquares ++ List(7 ~ 9,
                                                                      9 ~ 10,
                                                                      10 ~ 8,
                                                                      10 ~ 11,
                                                                      11 ~ 12,
                                                                      12 ~ 13,
                                                                      13 ~ 14,
                                                                      14 ~ 8,
                                                                      12 ~ 15,
                                                                      15 ~ 16,
                                                                      16 ~ 13,
                                                                      16 ~ 18,
                                                                      18 ~ 17,
                                                                      17 ~ 13,
                                                                      18 ~ 20,
                                                                      20 ~ 19,
                                                                      19 ~ 17)

  val edgeOverlap: Graph[Int, UnDiEdge] = preOverlap ++ List(20 ~ 22, 22 ~ 21, 21 ~ 19, 21 ~ 23, 23 ~ 24, 24 ~ 19)
  "A graph with an overlapping edge" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(edgeOverlap))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(edgeOverlap) should have message
    "Addition refused: " +
      "nodes = GraphPredef(15, 9, 1, 16, 2, 17, 24, 3, 18, 10, 4, 11, 12, 19, 13, 5, 20, 6, 21, 7, 22, 14, 8, 23)" + ", " +
      "edges = GraphPredef(15~16, 9~10, 1~2, 16~13, 16~18, 2~3, 2~5, 17~13, 24~19, 3~4, 18~20, 18~17, 10~11, 10~8, 4~1, 4~7, 11~12, 12~13, 12~15, 19~17, 13~14, 5~6, 20~22, 20~19, 6~3, 21~19, 21~23, 7~9, 7~8, 22~21, 14~8, 8~3, 23~24)"

  val areaOverlap3: Graph[Int, UnDiEdge] = preOverlap ++ List(19 ~ 21, 22 ~ 21, 21 ~ 17)
  "A graph with another overlapping area" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(areaOverlap3))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(areaOverlap3) should have message
    "Addition refused: " +
      "nodes = GraphPredef(15, 9, 1, 16, 2, 17, 3, 18, 10, 4, 11, 12, 19, 13, 5, 20, 6, 21, 7, 22, 14, 8)" + ", " +
      "edges = GraphPredef(15~16, 9~10, 1~2, 16~13, 16~18, 2~3, 2~5, 17~13, 3~4, 18~20, 18~17, 10~11, 10~8, 4~1, 4~7, 11~12, 12~13, 12~15, 19~21, 19~17, 13~14, 5~6, 20~19, 6~3, 21~17, 7~9, 7~8, 22~21, 14~8, 8~3)"

}
