package vision.id.tessella

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scalax.collection.GraphEdge.UnDiEdge

import vision.id.tessella.Alias.Tiling

class preAddTest extends FlatSpec with AddUtils {

  // ---------------- adding single node ----------------

  def oneTriangle: Tiling = Triangle.toTiling

  "Adding a single node to a valid tessellation" must "be NOT valid" in {
    assertThrows[IllegalArgumentException](oneTriangle + 4)
  }

  the[IllegalArgumentException] thrownBy oneTriangle + 4 should have message
    "Addition refused: " +
      "cannot add single node 4"

  // ---------------- adding single edge ----------------

  def fourSquares: Tiling = Tiling.fromVertex(Full.s("(4*4)"))

  "Adding a single edge not linking two perimeter nodes" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](fourSquares + Side(1, 3))
  }

  the[IllegalArgumentException] thrownBy fourSquares + Side(1, 3) should have message
    "Addition refused: " +
      "endpoints of single edge 1~3 must be both on perimeter"

  "Adding a single edge linking two perimeter nodes" can "divide an existing p-gon in a valid way" in {
    assert(
      (Tiling.fromVertex(Vertex.s("(4*3)")) + Side(1, 3)).edges.toString === "EdgeSet(1-2, 1=3, 5-6, 2-3, 6-7, 6=1, 3-4, 7-8, 4=1, 4-5, 8-1)")
  }

  it can "also divide an existing p-gon in NON valid way" in {
    assertThrows[IllegalArgumentException](fourSquares + Side(2, 4))
  }

  it can "create a new p-gon" in {
    val t: Tiling = Tiling.fromG(Tiling.squareNet(3, 2).toG - UnDiEdge(2, 3))
    assert(
      (t + Side(2, 3)).edges.toString === "EdgeSet(9-10, 1-2, 1-5, 5-9, 5=6, 2=6, 2-3, 6=7, 6=10, 3-4, 3=7, 10-11, 7=11, 7=8, 4-8, 11-12, 8-12)")
  }

  def uShape: Tiling = Tiling.fromG(Tiling.squareNet(3, 3).toG -- Set(UnDiEdge(2, 3), UnDiEdge(6, 7)))

  it can "also create a NON valid new p-gon" in {
    assertThrows[IllegalArgumentException](uShape + Side(2, 3))
  }

  // ---------------- adding multiple edges / nodes ----------------

  "Adding edges containing negative node values" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](oneTriangle ++ Set(Side(3, -4), Side(-4, 1)))
  }

  the[IllegalArgumentException] thrownBy oneTriangle ++ Set(Side(3, -4), Side(-4, 1)) should have message
    "Addition refused: " +
      "added non positive edges = Vector(3~-4, -4~1)"

  "Adding edges not forming a single path" can "be valid" in {
    assert(
      (oneTriangle ++ Set(Side(3, 4), Side(4, 1), Side(4, 5), Side(5, 3))).edges.toString === "EdgeSet(1-2, 5-3, 2-3, 3=1, 3=4, 4-1, 4-5)")
  }

  it can "also be NOT valid" in {
    assertThrows[IllegalArgumentException](
      oneTriangle ++ Set(Side(3, 4), Side(4, 1), Side(4, 5), Side(5, 3), Side(1, 5)))
  }

  "Adding edges forming a single path not linking two perimeter nodes" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](fourSquares ++ Set(Side(1, 10), Side(10, 3)))
  }

  the[IllegalArgumentException] thrownBy fourSquares ++ Set(Side(1, 10), Side(10, 3)) should have message
    "Addition refused: " +
      "endpoints of edges path must be both on perimeter Vector(1~10, 10~3)"

  "Adding edges forming a single path linking two perimeter nodes 'from the inside'" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](fourSquares ++ Set(Side(2, 10), Side(10, 11), Side(11, 4)))
  }

  the[IllegalArgumentException] thrownBy fourSquares ++ Set(Side(2, 10), Side(10, 11), Side(11, 4)) should have message
    "Addition refused: " +
      "endpoints of edges connecting 'from the inside' of the perimeter Vector(2~10, 10~11, 11~4)"

  "Adding edges forming a single path linking two perimeter adjacent nodes" can "be valid" in {
    assert((oneTriangle ++ List(Side(3, 4), Side(4, 1))).edges.toString === "EdgeSet(1-2, 2-3, 3=1, 3-4, 4-1)")
  }

  it can "also be NOT valid" in {
    assertThrows[IllegalArgumentException](uShape ++ Set(Side(2, 17), Side(17, 18), Side(18, 6)))
  }

  "Adding edges forming a single path linking two perimeter non adjacent nodes 'from the outside'" can "be valid" in {
    val t = Tiling.fromG(Tiling.hexagonNet(3, 2).toG - 4)
    assert(
      (t ++ List(Side(3, 4), Side(4, 5))).edges.toString === "EdgeSet(15-22, 9-16, 9=10, 1-8, 1-2, 16-17, 2-3, 17-18, 3=10, 3-4, 18-19, 10=11, 4-5, 11=18, 11=12, 12=13, 19-20, 13=20, 13=14, 5=12, 5-6, 20-21, 6-7, 21-22, 7-14, 14-15, 8-9)")
  }

  it can "also be NOT valid" in {
    assertThrows[IllegalArgumentException](uShape ++ Set(Side(2, 17), Side(17, 3)))
  }

}
