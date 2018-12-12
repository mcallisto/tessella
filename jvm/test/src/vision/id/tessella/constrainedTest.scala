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
      "graph not connected"

  val nodesWith1Edge: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4))

  "A graph with 1-degree nodes" can "NOT be a tessellation" in {
    assert((nodesWith1Edge get 4).degree === 1)
    assertThrows[IllegalArgumentException](Tiling.fromG(nodesWith1Edge))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(nodesWith1Edge) should have message
    "Addition refused: " +
      "nodes with wrong number of edges = Set(4)"

  val nodesWith7Edges: Graph[Int, UnDiEdge] = Graph.from(edges = List(2 ~ 3, 3 ~ 4, 4 ~ 5, 5 ~ 6, 6 ~ 7, 7 ~ 8, 8 ~ 2, 1 ~ 2, 1 ~ 3, 1 ~ 4, 1 ~ 5, 1 ~ 6, 1 ~ 7, 1 ~ 8))

  "A graph with a 7-degree node" can "NOT be a tessellation" in {
    assert((nodesWith7Edges get 1).degree === 7)
    assertThrows[IllegalArgumentException](Tiling.fromG(nodesWith7Edges))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(nodesWith7Edges) should have message
    "Addition refused: " +
      "nodes with wrong number of edges = Set(1)"

  val negativeTriangle: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ -2, -2 ~ 3, 3 ~ 1))

  "A graph with nodes having negative value" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(negativeTriangle))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(negativeTriangle) should have message
    "Addition refused: " +
      "non positive nodes = Set(-2)"

  val papillon: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 1, 2 ~ 4, 4 ~ 5, 5 ~ 2))

  "A graph with two non adjacent p-gons sharing a vertex" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(papillon))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(papillon) should have message
    "Addition refused: " +
      "perimeter is not a simple polygon"

  val threeAdjacentSquares: Graph[Int, UnDiEdge] =
    Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 1, 2 ~ 5, 5 ~ 6, 6 ~ 3, 4 ~ 7, 7 ~ 8, 8 ~ 3))

  val moreThanFullVertex: Graph[Int, UnDiEdge] = threeAdjacentSquares ++ List(8 ~ 9, 9 ~ 10, 10 ~ 6)
  "A graph with three squares and a pentagon at a vertex" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(moreThanFullVertex))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(moreThanFullVertex) should have message
    "Addition refused: " +
      "perimeter is not a simple polygon"

  val areaOverlap1: Graph[Int, UnDiEdge] = threeAdjacentSquares ++ List(8 ~ 9, 9 ~ 10, 10 ~ 11, 11 ~ 3)
  "A graph with three squares and a regular pentagon overlapped at a vertex" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(areaOverlap1))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(areaOverlap1) should have message
    "Addition refused: " +
      "perimeter is not a simple polygon"

  val sixSquares: Graph[Int, UnDiEdge] =
    threeAdjacentSquares ++ List(7 ~ 9, 9 ~ 10, 10 ~ 8, 10 ~ 11, 11 ~ 12, 12 ~ 8, 11 ~ 13, 13 ~ 14, 14 ~ 12)

  val vertexOverlapping: Graph[Int, UnDiEdge] = sixSquares ++ List(12 ~ 15, 15 ~ 16, 16 ~ 14)
  "A graph with an overlapping vertex" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(vertexOverlapping))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(vertexOverlapping) should have message
    "Addition refused: " +
      "perimeter is not a simple polygon"

  val areaOverlap2: Graph[Int, UnDiEdge] =
    sixSquares ++ List(12 ~ 15, 15 ~ 16, 16 ~ 17, 17 ~ 18, 18 ~ 19, 19 ~ 20, 20 ~ 14)
  "A graph with an overlapping area" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(areaOverlap2))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(areaOverlap2) should have message
    "Addition refused: " +
      "perimeter is not a simple polygon"

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
      "perimeter is not a simple polygon"

  val areaOverlap3: Graph[Int, UnDiEdge] = preOverlap ++ List(19 ~ 21, 21 ~ 22, 22 ~ 17)
  "A graph with another overlapping area" can "NOT be a tessellation" in {
    assertThrows[IllegalArgumentException](Tiling.fromG(areaOverlap3))
  }

  the[IllegalArgumentException] thrownBy Tiling.fromG(areaOverlap3) should have message
    "Addition refused: " +
      "perimeter is not a simple polygon"

}
