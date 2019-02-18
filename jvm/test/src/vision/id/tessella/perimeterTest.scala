package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.GraphPredef._

import vision.id.tessella.Tessella.Tiling
import vision.id.tessella.Cartesian2D.Point2D

import scala.util.Success

class perimeterTest extends FlatSpec with TilingUtils {

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

  "A perimeter" can "be checked if two nodes belong to one pgon" in {
    assert(fourSquares.getOnePolygonPerimeterPath(2, 4) === Some(Traversable(2 ~ 3, 3 ~ 4)))
  }
}
