package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.GraphPredef._

import vision.id.tessella.Tessella.Tiling
import vision.id.tessella.Cartesian2D.Point2D

import scala.util.Success

class perimeterTest extends FlatSpec with TilingUtils with Loggable {

  setLogLevel("WARN")

    val hive: Tiling = Tiling.hexagonNet(2, 2)

  "A 'hive' tessellation" must "have a perimeter length of 14 units" in {
    assert(hive.toPerimeterSimplePolygon match {
      case Success(polygon) => polygon.perimeter === 14.0
      case _                => false
    })
  }

  it must "have a perimeter represented by a simple polygon" in {
    assert(hive.toPerimeterSimplePolygon match {
      case Success(polygon) =>
        polygon.toPolyline2D().cs.map(new Point2D(_).toString) === List(
          "{0.0:0.0}",
          "{0.5:0.8660254}",
          "{0.0:1.73205081}",
          "{0.5:2.59807621}",
          "{0.0:3.46410162}",
          "{-1.0:3.46410162}",
          "{-1.5:4.33012702}",
          "{-2.5:4.33012702}",
          "{-3.0:3.46410162}",
          "{-2.5:2.59807621}",
          "{-3.0:1.73205081}",
          "{-2.5:0.8660254}",
          "{-1.5:0.8660254}",
          "{-1.0:0.0}"
        )
      case _ => false
    })
  }

  val periRx: Tiling = Tiling.fromSides(Set(1 ~ 2, 2 ~ 5, 5 ~ 7, 7 ~ 1).map(Side.fromEdge(_)))

  "A perimeter ordered to right" can "have its nodes ordered" in {
    assert(periRx.perimeterOrderedNodes === List(1, 2, 5, 7, 1))
  }

  it can "have its edges ordered" in {
    assert(periRx.perimeterOrderedEdges === List(1 ~ 2, 2 ~ 5, 5 ~ 7, 7 ~ 1))
  }

  val periLx: Tiling = Tiling.fromSides(Set(1 ~ 3, 3 ~ 5, 5 ~ 2, 2 ~ 1).map(Side.fromEdge(_)))

  "A perimeter ordered to left" can "have its nodes ordered" in {
    assert(periLx.perimeterOrderedNodes === List(1, 2, 5, 3, 1))
  }

  it can "have its edges ordered" in {
    assert(periLx.perimeterOrderedEdges === List(2 ~ 1, 5 ~ 2, 3 ~ 5, 1 ~ 3))
  }

  val fourSquares: Tiling = Tiling.fromVertex(Full.s("(4*4)"))

  "Several difficult perimeter cases" can "be identified" in {
    val case1: Tiling = Square.toTiling ++ Set(
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
    assert(case1.perimeterOrderedNodes === List(1, 2, 5, 7, 9, 10, 8, 6, 3, 4, 1))

    val case2: Tiling = Triangle.toTiling ++ Set(
      2 ~ 4,
      3 ~ 4,
      5 ~ 4,
      5 ~ 2,
      4 ~ 6,
      5 ~ 6
    ).map(Side.fromEdge(_))
    assert(case2.perimeterOrderedNodes === List(1, 2, 5, 6, 4, 3, 1))

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
    assert(case3.perimeterOrderedNodes === List(1, 2, 11, 13, 14, 12, 5, 7, 9, 10, 8, 6, 16, 18, 17, 15, 3, 4, 1))

    val case4: Tiling = Tiling.fromSides(
      Set(
        1 ~ 2,
        2 ~ 3,
        3 ~ 1,
        3 ~ 4,
        4 ~ 1,
        4 ~ 5,
        5 ~ 1,
        5 ~ 6,
        6 ~ 1,
        6 ~ 7,
        7 ~ 1
      ).map(Side.fromEdge(_)))
    assert(case4.perimeterOrderedNodes === List(1, 2, 3, 4, 5, 6, 7, 1))

    val case5: Tiling = Tiling.triangleNet(6, 2)
    assert(case5.perimeterOrderedNodes === List(1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 1))

    val case6: Tiling = Tiling.fromVertex(Vertex.s("(3*3.4*2)"))
    assert(case6.perimeterOrderedNodes === List(2, 3, 4, 5, 6, 7, 8, 2))
  }

}
