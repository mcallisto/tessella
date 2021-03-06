package vision.id.tessella

import scala.util.Try

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import vision.id.tessella.Tessella.Tiling

class preAddTest extends FlatSpec with AddUtils with Loggable {

  setLogLevel("WARN")

  // ---------------- adding single node ----------------

  def oneTriangle: Tiling = Triangle.toTiling

  "Adding a single node to a valid tessellation" must "be NOT valid" in {
    assertThrows[IllegalArgumentException](oneTriangle += 4)
  }

  the[IllegalArgumentException] thrownBy (oneTriangle += 4) should have message
    "Addition refused: " +
      "nodes = Set(4), edges = Set()"

  // ---------------- adding single edge ----------------

  def fourSquares: Tiling = Full.s("(4*4)").toTiling

  "Adding a single edge not linking two perimeter nodes" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](fourSquares + Side(1, 3))
  }

  the[IllegalArgumentException] thrownBy (fourSquares += Side(1, 3)) should have message
    "Addition refused: " +
      "nodes = Set(), edges = Set(1~3)"

  "Adding a single edge linking two perimeter nodes" can "divide an existing p-gon in a valid way" in {
    assert(
      (Vertex.s("(4*3)").toTiling += Side(1, 3)).edges.toString === "EdgeSet(1-2, 1=3, 5-6, 2-3, 6=1, 6-7, 3-4, 7-8, 4=1, 4-5, 8-1)")
  }

  it can "also divide an existing p-gon in NON valid way" in {
    assertThrows[IllegalArgumentException](fourSquares += Side(2, 4))
  }

  it can "create a new p-gon" in {
    val t: Tiling = Tiling.squareNet(3, 2) -= Side(2, 3)
    assert(
      (t += Side(2, 3)).edges.toString === "EdgeSet(9-10, 1-2, 1-5, 5-9, 5=6, 2=6, 2-3, 6=7, 6=10, 3-4, 3=7, 10-11, 7=11, 7=8, 4-8, 11-12, 8-12)")
  }

  def uShape: Tiling = Tiling.squareNet(3, 3) --= Set(Side(2, 3), Side(6, 7))

  it can "also create a NON valid new p-gon" in {
    assertThrows[IllegalArgumentException](uShape += Side(2, 3))
  }

  // ---------------- adding multiple edges / nodes ----------------

  "Adding edges containing negative node values" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](oneTriangle ++= Set(Side(3, -4), Side(-4, 1)))
  }

  the[IllegalArgumentException] thrownBy (oneTriangle ++= Set(Side(3, -4), Side(-4, 1))) should have message
    "Addition refused: " +
      "nodes = Set(-4), edges = Set(3~-4, -4~1)"

  "Adding edges not forming a single path" can "be valid" in {
    assert(
      (oneTriangle ++= Set(Side(3, 4), Side(4, 1), Side(4, 5), Side(5, 3))).edges.toString === "EdgeSet(1-2, 5-3, 2-3, 3=1, 3=4, 4-1, 4-5)")
  }

  it can "also be NOT valid" in {
    assertThrows[IllegalArgumentException](
      oneTriangle ++= Set(Side(3, 4), Side(4, 1), Side(4, 5), Side(5, 3), Side(1, 5)))
  }

  "Adding edges forming a single path not linking two perimeter nodes" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](fourSquares ++= Set(Side(1, 10), Side(10, 3)))
  }

  the[IllegalArgumentException] thrownBy (fourSquares ++= Set(Side(1, 10), Side(10, 3))) should have message
    "Addition refused: " +
      "nodes = Set(10), edges = Set(1~10, 10~3)"

  "Adding edges forming a single path linking two perimeter nodes 'from the inside'" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](fourSquares ++= Set(Side(2, 10), Side(10, 11), Side(11, 4)))
  }

  the[IllegalArgumentException] thrownBy (fourSquares ++= Set(Side(2, 10), Side(10, 11), Side(11, 4))) should have message
    "Addition refused: " +
      "nodes = Set(10, 11), edges = Set(2~10, 10~11, 11~4)"

  "Adding edges forming a single path linking two perimeter adjacent nodes" can "be valid" in {
    assert((oneTriangle ++= Set(Side(3, 4), Side(4, 1))).edges.toString === "EdgeSet(1-2, 2-3, 3=1, 3-4, 4-1)")
  }

  it can "also be NOT valid" in {
    assertThrows[IllegalArgumentException](uShape ++= Set(Side(2, 17), Side(17, 18), Side(18, 6)))
  }

  "Adding edges forming a single path linking two perimeter non adjacent nodes 'from the outside'" can "be valid" in {
    val t = Tiling.hexagonNet(3, 2) -= 4
    assert(
      (t ++= Set(Side(3, 4), Side(4, 5))).edges.toString === "EdgeSet(15-22, 9-16, 9=10, 1-8, 1-2, 16-17, 2-3, 17-18, 3=10, 3-4, 18-19, 10=11, 4-5, 11=18, 11=12, 12=13, 19-20, 13=20, 13=14, 5=12, 5-6, 20-21, 6-7, 21-22, 7-14, 14-15, 8-9)")
  }

  it can "also be NOT valid" in {
    assertThrows[IllegalArgumentException](uShape ++= Set(Side(2, 17), Side(17, 3)))
  }

  // ---------------- miscellaneous ----------------

  def isModificationSuccessful(t: Tiling, f: Tiling => Tiling): Boolean =
    Try(f(t)).isSuccess

  def isModified(t: Tiling, f: Tiling => Tiling): Boolean = {
    val c = t.clone()
    Try(f(t)) match {
      case _ => t !== c
    }
  }

  def isPerimeterModified(t: Tiling, f: Tiling => Tiling): Boolean = {

    def s(ti: Tiling): String = ti.edges.filter(_.isPerimeter.contains(true)).toString

    val c = t.clone()
    Try(f(t)) match {
      case _ => (s(t) !== s(c)) && { t.setPerimeter; s(t) === s(c) }
    }
  }

  "Success of '+=' add" must "modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ += Side(1, 3)
    assert(isModificationSuccessful(Vertex.s("(4*3)").toTiling, f) === true)
    assert(isModified(Vertex.s("(4*3)").toTiling, f) === true)
  }

  "Failure of '+=' add" must "NOT modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ += Side(1, 3)
    assert(isModificationSuccessful(fourSquares, f) === false)
    assert(isModified(fourSquares, f) === false)
    assert(isPerimeterModified(fourSquares, f) === false)
  }

  "Success of '++=' add" must "modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ ++= Set(Side(3, 4), Side(4, 1), Side(4, 5), Side(5, 3))
    assert(isModificationSuccessful(oneTriangle, f) === true)
    assert(isModified(oneTriangle, f) === true)
  }

  "Failure of '++=' add" must "NOT modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ ++= Set(Side(1, 10), Side(10, 3))
    assert(isModificationSuccessful(fourSquares, f) === false)
    assert(isModified(fourSquares, f) === false)
    assert(isPerimeterModified(fourSquares, f) === false)
  }

  // ---------------- rough case ----------------
  
  "Failure of '++=' add at preCheck" must "NOT modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ ++= Set(Side(6, 17), Side(17, 18), Side(18, 7))
    assert(isModificationSuccessful(uShape, f) === false)
    assert(isModified(uShape, f) === false)
    assert(isPerimeterModified(uShape, f) === false)
  }

  "Failure of '++=' add at postAdd" must "NOT modify the mutable Tiling, apart from perimeter flags" in {
    val f: Tiling => Tiling = _ ++= Set(Side(2, 17), Side(17, 18), Side(18, 6))
    assert(isModificationSuccessful(uShape, f) === false)
    assert(isModified(uShape, f) === false)
    assert(isPerimeterModified(uShape, f) === false)
  }

}
