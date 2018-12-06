package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.GraphPredef._ // shortcuts

import vision.id.tessella.Cartesian2D.Point2D
import vision.id.tessella.TessellGraph.Tessell

class tessellMapTest extends FlatSpec {

  "A 3x3 square net" can "be converted in a map of nodes with coords" in {
    val t = TessellGraph.squareNet(3, 3)
    assert(
      t.toTessellMap.toPrintable === Map(
        5  → "{0.0:1.0}",
        10 → "{1.0:2.0}",
        14 → "{1.0:3.0}",
        1  → "{0.0:0.0}",
        6  → "{1.0:1.0}",
        9  → "{0.0:2.0}",
        13 → "{0.0:3.0}",
        2  → "{1.0:0.0}",
        12 → "{3.0:2.0}",
        7  → "{2.0:1.0}",
        3  → "{2.0:0.0}",
        16 → "{3.0:3.0}",
        11 → "{2.0:2.0}",
        8  → "{3.0:1.0}",
        4  → "{3.0:0.0}",
        15 → "{2.0:3.0}"
      ))
  }

  "A 6x2 square net" can "be converted in a map of nodes with coords" in {
    val t = TessellGraph.squareNet(6, 2)
    assert(
      t.toTessellMap.toPrintable === Map(
        5  → "{4.0:0.0}",
        10 → "{2.0:1.0}",
        14 → "{6.0:1.0}",
        20 → "{5.0:2.0}",
        1  → "{0.0:0.0}",
        6  → "{5.0:0.0}",
        21 → "{6.0:2.0}",
        9  → "{1.0:1.0}",
        13 → "{5.0:1.0}",
        2  → "{1.0:0.0}",
        17 → "{2.0:2.0}",
        12 → "{4.0:1.0}",
        7  → "{6.0:0.0}",
        3  → "{2.0:0.0}",
        18 → "{3.0:2.0}",
        16 → "{1.0:2.0}",
        11 → "{3.0:1.0}",
        8  → "{0.0:1.0}",
        19 → "{4.0:2.0}",
        4  → "{3.0:0.0}",
        15 → "{0.0:2.0}"
      ))
  }

  "A 4x2 triangle net with a removed central point" can "be" in {
    val newg = TessellGraph.triangleNet(4, 2).graph - 5
    val t    = new TessellGraph(newg)
    assert(
      t.toTessellMap.toPrintable === Map(
        1 → "{0.0:0.0}",
        6 → "{1.5:0.8660254}",
        9 → "{1.0:1.73205081}",
        2 → "{1.0:0.0}",
        7 → "{-1.0:1.73205081}",
        3 → "{2.0:0.0}",
        8 → "{0.0:1.73205081}",
        4 → "{-0.5:0.8660254}"
      ))

  }

  "An 8x2 triangle net with two removed central points" can "be" in {
    val newg = TessellGraph.triangleNet(8, 2).graph - 7 - 9
    val t    = new TessellGraph(newg)
    assert(
      t.toTessellMap.toPrintable === Map(
        5  → "{4.0:0.0}",
        10 → "{3.5:0.8660254}",
        14 → "{2.0:1.73205081}",
        1  → "{0.0:0.0}",
        6  → "{-0.5:0.8660254}",
        13 → "{1.0:1.73205081}",
        2  → "{1.0:0.0}",
        12 → "{0.0:1.73205081}",
        3  → "{2.0:0.0}",
        11 → "{-1.0:1.73205081}",
        8  → "{1.5:0.8660254}",
        4  → "{3.0:0.0}",
        15 → "{3.0:1.73205081}"
      ))
  }

  val vertexCaseStudy: Tessell = new Tessell(
    TessellGraph.poly(6).graph + (1 ~ 7, 7 ~ 8, 8 ~ 2, 7 ~ 9, 9 ~ 1, 9 ~ 10, 10 ~ 6))
  val neigh2: List[(Int, List[Int])] = {
    import vertexCaseStudy.ExtNode
    (vertexCaseStudy.graph get 1).getNeighsPaths
  }

  "A mapped node" can "be completed if it has 2 or more mapped neighbors(2)" in {
    val tm = new TessellMap(
      Map(
        1 → new Point2D(0.0, 0.0),
        6 → new Point2D(1.0, 0.0),
        9 → new Point2D(0.0, 1.0)
      ))
    assert(
      tm.completeNode(1, neigh2).get.toPrintable === Map(
        1  → "{0.0:0.0}",
        6  → "{1.0:0.0}",
        9  → "{0.0:1.0}",
        2  → "{-0.5:-0.8660254}",
        7  → "{-0.8660254:0.5}",
        5  → "{1.5:-0.8660254}",
        10 → "{1.0:1.0}",
        3  → "{0.0:-1.73205081}",
        8  → "{-1.3660254:-0.3660254}",
        4  → "{1.0:-1.73205081}"
      ))
    // mirrored on the x axis
    val xm = new TessellMap(
      Map(
        1 → new Point2D(0.0, 0.0),
        6 → new Point2D(-1.0, 0.0),
        9 → new Point2D(0.0, 1.0)
      ))
    assert(
      xm.completeNode(1, neigh2).get.toPrintable === Map(
        1  → "{0.0:0.0}",
        6  → "{-1.0:0.0}",
        9  → "{0.0:1.0}",
        2  → "{0.5:-0.8660254}",
        7  → "{0.8660254:0.5}",
        5  → "{-1.5:-0.8660254}",
        10 → "{-1.0:1.0}",
        3  → "{0.0:-1.73205081}",
        8  → "{1.3660254:-0.3660254}",
        4  → "{-1.0:-1.73205081}"
      ))
    // mirrored on the y axis
    val ym = new TessellMap(
      Map(
        1 → new Point2D(0.0, 0.0),
        6 → new Point2D(1.0, 0.0),
        9 → new Point2D(0.0, -1.0)
      ))
    assert(
      ym.completeNode(1, neigh2).get.toPrintable === Map(
        1  → "{0.0:0.0}",
        6  → "{1.0:0.0}",
        9  → "{0.0:-1.0}",
        2  → "{-0.5:0.8660254}",
        7  → "{-0.8660254:-0.5}",
        5  → "{1.5:0.8660254}",
        10 → "{1.0:-1.0}",
        3  → "{0.0:1.73205081}",
        8  → "{-1.3660254:0.3660254}",
        4  → "{1.0:1.73205081}"
      ))
    val tm1 = new TessellMap(
      Map(
        1 → new Point2D(0.0, 0.0),
        7 → new Point2D(-0.8660254, 0.5),
        9 → new Point2D(0.0, 1.0)
      ))
    assert(
      tm1.completeNode(1, neigh2).get.toPrintable === Map(
        1  → "{0.0:0.0}",
        6  → "{1.0:0.0}",
        9  → "{0.0:1.0}",
        2  → "{-0.5:-0.8660254}",
        7  → "{-0.8660254:0.5}",
        5  → "{1.5:-0.8660254}",
        10 → "{1.0:1.0}",
        3  → "{0.0:-1.73205081}",
        8  → "{-1.3660254:-0.3660254}",
        4  → "{1.0:-1.73205081}"
      ))
  }

  "A starting map" can "be created" in {
    assert(
      vertexCaseStudy.toTessellMap.toPrintable === Map(
        5  → "{0.0:1.73205081}",
        10 → "{-1.3660254:0.3660254}",
        1  → "{0.0:0.0}",
        6  → "{-0.5:0.8660254}",
        9  → "{-0.8660254:-0.5}",
        2  → "{1.0:0.0}",
        7  → "{0.0:-1.0}",
        3  → "{1.5:0.8660254}",
        8  → "{1.0:-1.0}",
        4  → "{1.0:1.73205081}"
      ))
    assert(
      TessellGraph.squareNet(3, 2).toTessellMap.toPrintable === Map(
        5  → "{0.0:1.0}",
        10 → "{1.0:2.0}",
        1  → "{0.0:0.0}",
        6  → "{1.0:1.0}",
        9  → "{0.0:2.0}",
        2  → "{1.0:0.0}",
        12 → "{3.0:2.0}",
        7  → "{2.0:1.0}",
        3  → "{2.0:0.0}",
        11 → "{2.0:2.0}",
        8  → "{3.0:1.0}",
        4  → "{3.0:0.0}"
      ))
    assert(
      TessellGraph.triangleNet(6, 2).toTessellMap.toPrintable === Map(
        5  → "{-0.5:0.8660254}",
        10 → "{0.0:1.73205081}",
        1  → "{0.0:0.0}",
        6  → "{0.5:0.8660254}",
        9  → "{-1.0:1.73205081}",
        2  → "{1.0:0.0}",
        12 → "{2.0:1.73205081}",
        7  → "{1.5:0.8660254}",
        3  → "{2.0:0.0}",
        11 → "{1.0:1.73205081}",
        8  → "{2.5:0.8660254}",
        4  → "{3.0:0.0}"
      ))
    assert(
      TessellGraph.hexagonNet(2, 3).toTessellMap.toPrintable === Map(
        5  → "{3.0:1.73205081}",
        10 → "{2.5:2.59807621}",
        14 → "{1.0:3.46410162}",
        20 → "{1.0:5.19615242}",
        1  → "{0.0:0.0}",
        6  → "{-0.5:0.8660254}",
        21 → "{1.5:6.06217783}",
        9  → "{1.5:2.59807621}",
        13 → "{0.0:3.46410162}",
        2  → "{1.0:0.0}",
        17 → "{3.0:5.19615242}",
        22 → "{2.5:6.06217783}",
        12 → "{-0.5:2.59807621}",
        7  → "{0.0:1.73205081}",
        3  → "{1.5:0.8660254}",
        18 → "{-0.5:4.33012702}",
        16 → "{2.5:4.33012702}",
        11 → "{3.0:3.46410162}",
        8  → "{1.0:1.73205081}",
        19 → "{0.0:5.19615242}",
        4  → "{2.5:0.8660254}",
        15 → "{1.5:4.33012702}"
      ))
  }
}
