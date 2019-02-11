package vision.id.tessella

import scala.util.Try

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import vision.id.tessella.Tessella.Tiling

class preSubtractTest extends FlatSpec with AddUtils with Loggable {

  setLogLevel("WARN")

  // ---------------- subtracting single node ----------------

  "Subtracting a 6-degree node" must "be valid" in {
    assert((Tiling.fromVertex(Full.s("(3*6)")) -= 1).edges.toString === "EdgeSet(5-6, 2-3, 6-7, 3-4, 7-2, 4-5)")
  }

  "Subtracting the only node not on the perimeter" can "be valid" in {
    assert(
      (Tiling.fromVertex(Full.s("(3*3.4*2)")) -= 1).edges.toString === "EdgeSet(5-6, 2-3, 6-7, 3-4, 7-8, 4-5, 8-2)")
  }

  def octagonFull: Tiling = Tiling.fromVertex(Full.s("(4.8*2)"))

  it can "also be NOT valid" in {
    assertThrows[IllegalArgumentException](octagonFull -= 1)
  }

  the[IllegalArgumentException] thrownBy (octagonFull -= 1) should have message
    "Subtraction refused: " +
      "nodes = Set(1), edges = Set()"

  "Subtracting a perimeter node adjacent to another 2-degree node" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](octagonFull -= 2)
  }

  the[IllegalArgumentException] thrownBy (octagonFull -= 2) should have message
    "Subtraction refused: " +
      "nodes = Set(2), edges = Set()"

  def fourSquares: Tiling = Tiling.fromVertex(Full.s("(4*4)"))

  "Subtracting a perimeter node adjacent only to > 2-degree nodes" can "be valid" in {
    assert((fourSquares -= 9).edges.toString === "EdgeSet(1-2, 5-6, 2-3, 6=1, 6-7, 3-4, 7-8, 4=1, 4-5, 8-1)")
  }

  it can "be NOT valid" in {
    assertThrows[IllegalArgumentException](Tiling.fromVertex(Vertex.s("(4*3)")) -= 5)
  }

  // ---------------- subtracting single edge ----------------

  "Subtracting an edge whose endpoints are not on the perimeter" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](octagonFull -= Side(1, 2))
  }

  the[IllegalArgumentException] thrownBy (octagonFull -= Side(1, 2)) should have message
    "Subtraction refused: " +
      "nodes = Set(), edges = Set(1~2)"

  "Subtracting an non perimeter edge whose endpoints are on the perimeter" can "be valid" in {
    assert((Tiling.fromVertex(Vertex.s("(3*2)")) -= Side(1, 3)) === Square.toTiling)
  }

  it can "also be NOT valid" in {
    assertThrows[IllegalArgumentException](fourSquares -= Side(1, 2))
  }

  "Subtracting a perimeter edge with a 2-degree node" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](fourSquares -= Side(2, 3))
  }

  the[IllegalArgumentException] thrownBy (fourSquares -= Side(2, 3)) should have message
    "Subtraction refused: " +
      "nodes = Set(), edges = Set(2~3)"

  "Subtracting a perimeter edge with > 2-degree nodes" must "be valid" in {
    assert(
      (Tiling.fromVertex(Full.s("(3*3.4*2)")) -= Side(3, 4)).edges.toString === "EdgeSet(1=2, 5=1, 5-6, 2-3, 6-7, 3-1, 7=1, 7-8, 4-1, 4-5, 8-2)")
  }

  // ---------------- subtracting multiple edges / nodes ----------------

  "Subtracting all nodes" must "be valid" in {
    assert((Square.toTiling --= Set(1, 2, 3, 4)).edges.toString === "EdgeSet()")
  }

  "Subtracting perimeter nodes not forming a single path" can "be valid" in {
    val t = Tiling.squareNet(3, 2)
    assert(
      (t --= Set(1, 4)).edges.toString === "EdgeSet(9-10, 5-9, 5-6, 2-3, 2-6, 6=7, 6=10, 3-7, 10-11, 7=11, 7-8, 11-12, 8-12)")
  }

  it can "also be NOT valid" in {
    assertThrows[IllegalArgumentException](fourSquares --= Set(3, 7))
  }

  "Subtracting perimeter 2-degree nodes forming a single path adjacent to another 2-degree node" must "NOT be valid" in {
    assertThrows[IllegalArgumentException](octagonFull --= Set(5, 6))
  }

  the[IllegalArgumentException] thrownBy (octagonFull --= Set(5, 6)) should have message
    "Subtraction refused: " +
      "nodes = Set(5, 6), edges = Set()"

  "Subtracting perimeter 2-degree nodes forming a single path not adjacent only to > 2-degree nodes" must "be valid" in {
    assert(
      (octagonFull --= Set(5, 6, 7, 8, 9)).edges.toString === "EdgeSet(15-2, 1=2, 2-3, 3-4, 10-1, 10-11, 4-1, 11-12, 12-13, 13-14, 14-15)")
  }

  "Subtracting non perimeter nodes" can "be valid" in {
    assert(
      (Tiling.squareNet(3, 2) --= Set(6, 7)).edges.toString === "EdgeSet(9-10, 1-2, 1-5, 5-9, 2-3, 3-4, 10-11, 4-8, 11-12, 8-12)")
  }

  it can "also be NOT valid" in {
    assertThrows[IllegalArgumentException](Tiling.squareNet(4, 2) --= Set(7, 8))
  }

  // ---------------- miscellaneous ----------------

  def fourSquaresNet: Tiling = Tiling.squareNet(2, 2)

  def threeSquares: Tiling = fourSquaresNet - 9

  def isModificationSuccessful(t: Tiling, f: Tiling => Tiling): Boolean =
    Try(f(t)).isSuccess

  def isModified(t: Tiling, f: Tiling => Tiling): Boolean = {
    val c = t.clone()
    Try(f(t)) match {
      case _ => t !== c
    }
  }

  "Success of '-=' subtract" must "modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ -= 9
    assert(isModificationSuccessful(fourSquaresNet, f) === true)
    assert(isModified(fourSquaresNet, f) === true)
  }

  "Failure of '-=' subtract" must "anyway modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ -= 1
    assert(isModificationSuccessful(threeSquares, f) === false)
    assert(isModified(threeSquares, f) === true)
  }

  "Success of '-' subtract" must "NOT modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ - 9
    assert(isModificationSuccessful(fourSquaresNet, f) === true)
    assert(isModified(fourSquaresNet, f) === false)
  }

  "Failure of '-' subtract" must "NOT modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ - 1
    assert(isModificationSuccessful(threeSquares, f) === false)
    assert(isModified(threeSquares, f) === false)
  }

  "Success of '--=' subtract" must "modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ --= Set(9)
    assert(isModificationSuccessful(fourSquaresNet, f) === true)
    assert(isModified(fourSquaresNet, f) === true)
  }

  "Failure of '--=' subtract" must "modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ --= Set(1)
    assert(isModificationSuccessful(threeSquares, f) === false)
    assert(isModified(threeSquares, f) === true)
  }

  "Success of '--' subtract" must "NOT modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ -- Set(9)
    assert(isModificationSuccessful(fourSquaresNet, f) === true)
    assert(isModified(fourSquaresNet, f) === false)
  }

  "Failure of '--' subtract" must "NOT modify the mutable Tiling" in {
    val f: Tiling => Tiling = _ -- Set(1)
    assert(isModificationSuccessful(threeSquares, f) === false)
    assert(isModified(threeSquares, f) === false)
  }

}
