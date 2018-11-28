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

  val d: Tessell           = new Tessell(TessellGraph.squareNet(3, 3).graph - (5 ~ 9, 6 ~ 10))
  val e: d.perimeter.EdgeT = d.perimeter get (5 ~ 6)
  //saveFilePretty(draw(d, labelStyle = 2, markStyle = 2), imagePath + "test/uncomplete")

  val periRx: Tessell = new Tessell(Graph(1 ~ 2, 2 ~ 5, 5 ~ 7, 7 ~ 1))

  "A perimeter ordered to right" can "have its nodes ordered" in {
    assert(periRx.orderedNodes.map(_.toOuter) === List(1, 2, 5, 7, 1))
  }

  it can "have its edges ordered" in {
    assert(periRx.orderedEdges === List(1 ~ 2, 2 ~ 5, 5 ~ 7, 7 ~ 1))
  }

  val r2: periRx.perimeter.NodeT = periRx.perimeter get 2
  val r5: periRx.perimeter.NodeT = periRx.perimeter get 5
  val r7: periRx.perimeter.NodeT = periRx.perimeter get 7

  it can "have its ordered nodes starting from a given node" in {
    val ns = {
      import periRx.PeriNode
      r5.startNodes()
    }
    assert(ns.map(_.toOuter) === List(5, 7, 1, 2))
  }

  it can "be queried for the ordered neighbors of a given node" in {
    val ns = {
      import periRx.PeriNode
      r5.nodeNeighbors
    }
    assert(ns === (r2, r7))
  }

  it can "be queried for the other ordered neighbor of a given node" in {
    val n = {
      import periRx.PeriNode
      r5.otherNeighbor(r7)
    }
    assert(n === r2)
  }

  it can "be queried for the minor ordered neighbor of a given node" in {
    val n = {
      import periRx.PeriNode
      r5.minNeighbor
    }
    assert(n === r2)
  }

  val periLx: Tessell = new Tessell(Graph(1 ~ 3, 3 ~ 5, 5 ~ 2, 2 ~ 1))

  "A perimeter ordered to left" can "have its nodes ordered" in {
    assert(periLx.orderedNodes.map(_.toOuter) === List(1, 2, 5, 3, 1))
  }

  it can "have its edges ordered" in {
    assert(periLx.orderedEdges === List(1 ~ 2, 2 ~ 5, 5 ~ 3, 3 ~ 1))
  }

  val l1: periLx.perimeter.NodeT = periLx.perimeter get 1
  val l2: periLx.perimeter.NodeT = periLx.perimeter get 2
  val l3: periLx.perimeter.NodeT = periLx.perimeter get 3
  val l5: periLx.perimeter.NodeT = periLx.perimeter get 5

  it can "be queried for the ordered endpoint nodes of a given edge" in {
    val ns = {
      import periLx.PeriEdge
      (periLx.perimeter get (5 ~ 2)).orderedEndPoints
    }
    assert(ns === (l2, l5))
  }

  it can "have its ordered nodes starting from a given node" in {
    val ns = {
      import periLx.PeriNode
      l3.startNodes()
    }
    assert(ns.map(_.toOuter) === List(3, 1, 2, 5))
  }

  it can "be queried for the ordered neighbors of a given node" in {
    val ns = {
      import periLx.PeriNode
      l3.nodeNeighbors
    }
    assert(ns === (l5, l1))
  }

  it can "be queried for the other ordered neighbor of a given node" in {
    val n = {
      import periLx.PeriNode
      l3.otherNeighbor(l5)
    }
    assert(n === l1)
  }

  it can "be queried for the minor ordered neighbor of a given node" in {
    val n = {
      import periLx.PeriNode
      l3.minNeighbor
    }
    assert(n === l1)
  }

}
