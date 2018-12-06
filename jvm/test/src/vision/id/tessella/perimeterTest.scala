package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.Graph
import scalax.collection.GraphPredef._

import vision.id.tessella.TessellGraph.Tessell
import vision.id.tessella.Cartesian2D.Point2D

class perimeterTest extends FlatSpec with SVG {

  val hive: Tessell = TessellGraph.hexagonNet(2, 2)

  "A 'hive' tessellation" must "have a perimeter length of 14 units" in {
    assert(hive.polygon.perimeter === 14.0)
  }

  it must "have a perimeter represented by a simple polygon" in {
    assert(
      hive.polygon.toPolyline2D().cs.map(new Point2D(_).toString) === List(
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
      ))
  }

  val periRx: Tessell = new Tessell(Graph(1 ~ 2, 2 ~ 5, 5 ~ 7, 7 ~ 1))

  "A perimeter ordered to right" can "have its nodes ordered" in {
    assert(periRx.periNodes === List(1, 2, 5, 7, 1))
  }

  it can "have its edges ordered" in {
    assert(periRx.periEdges === List(1 ~ 2, 2 ~ 5, 5 ~ 7, 7 ~ 1))
  }

  val periLx: Tessell = new Tessell(Graph(1 ~ 3, 3 ~ 5, 5 ~ 2, 2 ~ 1))

  "A perimeter ordered to left" can "have its nodes ordered" in {
    assert(periLx.periNodes === List(1, 2, 5, 3, 1))
  }

  it can "have its edges ordered" in {
    assert(periLx.periEdges === List(1 ~ 2, 2 ~ 5, 5 ~ 3, 3 ~ 1))
  }

}
