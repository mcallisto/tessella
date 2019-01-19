package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.GraphPredef._

import vision.id.tessella.Alias.Tiling

class neighborsTest extends FlatSpec with TilingUtils with Symmetry {

  val vertexFortyTwo: Tiling =
    Gon42.toTiling ++ Set(1 ~ 43, 2 ~ 43, 3 ~ 44, 44 ~ 45, 45 ~ 46, 46 ~ 47, 47 ~ 43).map(Side.fromEdge(_))

  "Node neighbors" can "be found for perimeter nodes" in {
    val vertexOtherCase =
      Tiling.fromSides(Set(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 1, 5 ~ 4, 5 ~ 1, 6 ~ 5, 6 ~ 1, 7 ~ 2, 7 ~ 1).map(Side.fromEdge(_)))
    assert(
      vertexOtherCase.outerNodeHood(vertexOtherCase get 1, vertexOtherCase.perimeterOrderedNodes) === List(
        (6, List(5)),
        (5, List(4)),
        (4, List(3, 2)),
        (2, List(7)),
        (7, List())
      ))

    val neighsPeri =
      vertexFortyTwo.outerNodeHood(vertexFortyTwo get 10, vertexFortyTwo.perimeterOrderedNodes)
    assert(
      neighsPeri === List(
        (9,
         List(8, 7, 6, 5, 4, 3, 2, 1, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23,
           22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11)),
        (11, List())
      ))

  }

  they can "be found for full nodes" in {

    val vertexCaseStudy: Tiling =
      Hexagon.toTiling ++ Set(1 ~ 7, 7 ~ 8, 8 ~ 2, 7 ~ 9, 9 ~ 1, 9 ~ 10, 10 ~ 6).map(Side.fromEdge(_))
    assert(
      vertexCaseStudy.outerNodeHood(vertexCaseStudy get 1, vertexCaseStudy.perimeterOrderedNodes) === List(
        (2, List(3, 4, 5, 6)),
        (6, List(10, 9)),
        (9, List(7)),
        (7, List(8, 2))
      ))

    val neighsFull1 =
      vertexFortyTwo.outerNodeHood(vertexFortyTwo get 2, vertexFortyTwo.perimeterOrderedNodes)
    assert(
      neighsFull1 === List(
        (1,
         List(42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17,
           16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3)),
        (3, List(44, 45, 46, 47, 43)),
        (43, List(1))
      ))

    val hex         = Tiling.hexagonNet(2, 3)
    val neighsFull2 = hex.outerNodeHood(hex get 15, hex.perimeterOrderedNodes)
    assert(
      neighsFull2 === List(
        (14, List(9, 10, 11, 16)),
        (16, List(17, 22, 21, 20)),
        (20, List(19, 18, 13, 14))
      ))

  }

  they can "be found for full nodes in edge case #18" in {
    val edgeCase = Tiling.fromSides(
      Set(
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
      ).map(Side.fromEdge(_)))
    val neighsFull = edgeCase.outerNodeHood(edgeCase get 3, edgeCase.perimeterOrderedNodes)
    assert(
      neighsFull === List(
        (1, List(2)),
        (2, List(9, 10)),
        (10, List(19, 18, 17, 16, 15, 14, 13, 12, 11, 4)),
        (4, List(1))
      ))
  }

}
