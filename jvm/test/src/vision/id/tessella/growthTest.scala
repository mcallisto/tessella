package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.Graph
import scalax.collection.GraphPredef._

import vision.id.tessella.Tessella.Tiling

class growthTest extends FlatSpec with TilingUtils with TryUtils {

  "A vertex" can "be grown into a tessellation" in {
    val result = Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1, 7 ~ 8, 8 ~ 2)
    assert(Tiling.fromVertex(Vertex.s("(3*2.4.3.4)")).toG === result)
  }

  it can "be grown into a collection of tessellations adding one polygon at a time" in {
    val result = List(
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1, 7 ~ 8, 8 ~ 2)
    )
    assert(Tiling.scanVertex(Vertex.s("(3*2.4.3.4)")).map(_.toG) === result)
  }

  "A full vertex" can "be grown into a tessellation" in {
    assert(
      Tiling.expandPattern(Full.s("(3*2.4.3.4)"), 20).safeGet.toG === Graph(
        1 ~ 2,
        2 ~ 3,
        2 ~ 9,
        2 ~ 11,
        3 ~ 1,
        3 ~ 4,
        4 ~ 1,
        4 ~ 5,
        4 ~ 14,
        5 ~ 6,
        5 ~ 15,
        5 ~ 16,
        6 ~ 1,
        6 ~ 7,
        6 ~ 18,
        7 ~ 1,
        7 ~ 8,
        7 ~ 20,
        8 ~ 2,
        9 ~ 10,
        10 ~ 3,
        10 ~ 12,
        11 ~ 8,
        11 ~ 9,
        12 ~ 3,
        12 ~ 13,
        13 ~ 4,
        14 ~ 5,
        14 ~ 13,
        15 ~ 6,
        16 ~ 15,
        16 ~ 17,
        17 ~ 14,
        18 ~ 7,
        18 ~ 19,
        19 ~ 15,
        19 ~ 22,
        20 ~ 8,
        20 ~ 21,
        21 ~ 18,
        22 ~ 18
      ))
  }

}
