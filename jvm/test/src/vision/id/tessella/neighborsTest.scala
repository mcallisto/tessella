package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.Graph
import scalax.collection.GraphPredef._

import vision.id.tessella.TessellGraph.Tessell

class neighborsTest extends FlatSpec with Symmetry {

  val vertexFortyTwo: Tessell = new Tessell(
    TessellGraph.poly(42).graph + (1 ~ 43, 2 ~ 43, 3 ~ 44, 44 ~ 45, 45 ~ 46, 46 ~ 47, 47 ~ 43))

  "Node neighbors" can "be found for perimeter nodes" in {
    val vertexOtherCase = new Tessell(Graph(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 1, 5 ~ 4, 5 ~ 1, 6 ~ 5, 6 ~ 1, 7 ~ 2, 7 ~ 1))
    import vertexOtherCase.ExtNode
    assert(
      (vertexOtherCase.graph get 1).perimeterNodeNeighbors === List(
        (6, List(5)),
        (5, List(4)),
        (4, List(3, 2)),
        (2, List(7)),
        (7, List())
      ))

    val neighsPeri = {
      import vertexFortyTwo.ExtNode
      (vertexFortyTwo.graph get 10).perimeterNodeNeighbors
    }
    assert(
      neighsPeri === List(
        (9,
         List(8, 7, 6, 5, 4, 3, 2, 1, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23,
           22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11)),
        (11, List())
      ))

  }

  they can "be found for full nodes" in {

    val vertexCaseStudy: Tessell =
      new Tessell(TessellGraph.poly(6).graph + (1 ~ 7, 7 ~ 8, 8 ~ 2, 7 ~ 9, 9 ~ 1, 9 ~ 10, 10 ~ 6))
    import vertexCaseStudy.ExtNode
    assert(
      (vertexCaseStudy.graph get 1).fullNodeNeighbors === List(
        (2, List(3, 4, 5, 6)),
        (6, List(10, 9)),
        (9, List(7)),
        (7, List(8, 2))
      ))

    val neighsFull1 = {
      import vertexFortyTwo.ExtNode
      (vertexFortyTwo.graph get 2).fullNodeNeighbors
    }
    assert(
      neighsFull1 === List(
        (1,
         List(42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17,
           16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3)),
        (3, List(44, 45, 46, 47, 43)),
        (43, List(1))
      ))

    val hex = TessellGraph.hexagonNet(2, 3)
    val neighsFull2 = {
      import hex.ExtNode
      (hex.graph get 15).fullNodeNeighbors
    }
    assert(
      neighsFull2 === List(
        (14, List(9, 10, 11, 16)),
        (16, List(17, 22, 21, 20)),
        (20, List(19, 18, 13, 14))
      ))

  }

  they can "be found for full nodes in edge case #18" in {
    val edgeCase = new Tessell(
      Graph(
        1 ~ 2,
        2 ~ 3,
        2 ~ 9,
        2 ~ 20,
        3 ~ 1,
        3 ~ 4,
        4 ~ 1,
        4 ~ 5,
        4 ~ 11,
        5 ~ 6,
        6 ~ 1,
        6 ~ 7,
        7 ~ 1,
        7 ~ 8,
        8 ~ 2,
        9 ~ 10,
        9 ~ 21,
        10 ~ 3,
        11 ~ 12,
        12 ~ 13,
        13 ~ 14,
        14 ~ 15,
        15 ~ 16,
        16 ~ 17,
        17 ~ 18,
        18 ~ 19,
        19 ~ 10,
        20 ~ 8,
        20 ~ 9,
        20 ~ 22,
        21 ~ 10,
        22 ~ 23,
        23 ~ 9,
        23 ~ 21
      ))
    val neighsFull = {
      import edgeCase.ExtNode
      (edgeCase.graph get 3).fullNodeNeighbors
    }
    assert(
      neighsFull === List(
        (1, List(2)),
        (2, List(9, 10)),
        (10, List(19, 18, 17, 16, 15, 14, 13, 12, 11, 4)),
        (4, List(1))
      ))
  }

}
