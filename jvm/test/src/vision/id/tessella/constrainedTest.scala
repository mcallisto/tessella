package vision.id.tessella

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

import vision.id.tessella.Alias.Tiling

class constrainedTest extends FlatSpec with GraphUtils with TilingUtils with Visualizer {

  val oneTriangle: Tiling = Triangle.toTiling

  "A regular triangle" can "be a tessellation" in {
    assert(oneTriangle.edges.toList === List(1 ~ 2, 2 ~ 3, 3 ~ 1))
  }

  "An addition to a valid tessellation" can "NOT be a single node" in {
    assertThrows[IllegalArgumentException](oneTriangle + 4)
  }

  the[IllegalArgumentException] thrownBy oneTriangle + 4 should have message
    "Addition refused: " +
      "cannot add a single node"

  val negative: Graph[Int, UnDiEdge] = oneTriangle.toG ++ List(3 ~ -4, -4 ~ 1)

  given(negative) { g =>
    it must "be with positive nodes" in {
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "non positive nodes = Set(-4)"
  }

  val disconnected: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 4))

  given(disconnected) { g =>
    "A disconnected graph" can "NOT be a tessellation" in {
      assert(g.isConnected === false)
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "graph not connected"
  }

  val nodesWith1Edge: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4))

  given(nodesWith1Edge) { g =>
    "A graph with 1-degree nodes" can "NOT be a tessellation" in {
      assert((g get 4).degree === 1)
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "nodes with wrong number of edges = Set(4)"
  }

  val nodesWith7Edges: Graph[Int, UnDiEdge] = Graph.from(
    edges = List(2 ~ 3, 3 ~ 4, 4 ~ 5, 5 ~ 6, 6 ~ 7, 7 ~ 8, 8 ~ 2, 1 ~ 2, 1 ~ 3, 1 ~ 4, 1 ~ 5, 1 ~ 6, 1 ~ 7, 1 ~ 8))

  given(nodesWith7Edges) { g =>
    "A graph with a 7-degree node" can "NOT be a tessellation" in {
      assert((g get 1).degree === 7)
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "nodes with wrong number of edges = Set(1)"
  }

  val negativeTriangle: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ -2, -2 ~ 3, 3 ~ 1))

  given(negativeTriangle) { g =>
    "A graph with nodes having negative value" can "NOT be a tessellation" in {
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "non positive nodes = Set(-2)"
  }

  val papillon: Graph[Int, UnDiEdge] = Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 1, 2 ~ 4, 4 ~ 5, 5 ~ 2))

  given(papillon) { g =>
    "A graph with two non adjacent p-gons sharing a vertex" can "NOT be a tessellation" in {
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "could not build perimeter"
  }

  val threeAdjacentSquares: Graph[Int, UnDiEdge] =
    Graph.from(edges = List(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 1, 2 ~ 5, 5 ~ 6, 6 ~ 3, 4 ~ 7, 7 ~ 8, 8 ~ 3))

  val moreThanFullVertex: Graph[Int, UnDiEdge] = threeAdjacentSquares ++ List(8 ~ 9, 9 ~ 10, 10 ~ 6)

  given(moreThanFullVertex) { g =>
    "A graph with three squares and a pentagon at a vertex" can "NOT be a tessellation" in {
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "perimeter is not a simple polygon"
  }

  val areaOverlap1: Graph[Int, UnDiEdge] = threeAdjacentSquares ++ List(8 ~ 9, 9 ~ 10, 10 ~ 11, 11 ~ 3)

  given(areaOverlap1) { g =>
    "A graph with three squares and a regular pentagon overlapped at a vertex" can "NOT be a tessellation" in {
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "perimeter is not a simple polygon"
  }

  val sixSquares: Graph[Int, UnDiEdge] =
    threeAdjacentSquares ++ List(7 ~ 9, 9 ~ 10, 10 ~ 8, 10 ~ 11, 11 ~ 12, 12 ~ 8, 11 ~ 13, 13 ~ 14, 14 ~ 12)

  val vertexOverlapping: Graph[Int, UnDiEdge] = sixSquares ++ List(12 ~ 15, 15 ~ 16, 16 ~ 14)

  given(vertexOverlapping) { g =>
    "A graph with an overlapping vertex" can "NOT be a tessellation" in {
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "perimeter is not a simple polygon"
  }

  val areaOverlap2: Graph[Int, UnDiEdge] =
    sixSquares ++ List(12 ~ 15, 15 ~ 16, 16 ~ 17, 17 ~ 18, 18 ~ 19, 19 ~ 20, 20 ~ 14)

  given(areaOverlap2) { g =>
    "A graph with an overlapping area" can "NOT be a tessellation" in {
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "perimeter is not a simple polygon"
  }

  val gap: Graph[Int, UnDiEdge] = sixSquares ++ List(6 ~ 15,
                                                     15 ~ 14,
                                                     5 ~ 16,
                                                     16 ~ 15,
                                                     16 ~ 20,
                                                     20 ~ 19,
                                                     19 ~ 15,
                                                     19 ~ 18,
                                                     18 ~ 14,
                                                     18 ~ 17,
                                                     17 ~ 13)

  given(gap) { g =>
    "A graph with a gap" can "NOT be a tessellation" in {
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "tiling with gap"
  }

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

  given(edgeOverlap) { g =>
    "A graph with an overlapping edge" can "NOT be a tessellation" in {
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "perimeter is not a simple polygon"
  }

  val areaOverlap3: Graph[Int, UnDiEdge] = preOverlap ++ List(19 ~ 21, 21 ~ 22, 22 ~ 17)

  given(areaOverlap3) { g =>
    "A graph with another overlapping area" can "NOT be a tessellation" in {
      assertThrows[IllegalArgumentException](Tiling.fromSides(g.toSides))
    }

    the[IllegalArgumentException] thrownBy Tiling.fromSides(g.toSides) should have message
      "Addition refused: " +
        "perimeter is not a simple polygon"
  }
}
