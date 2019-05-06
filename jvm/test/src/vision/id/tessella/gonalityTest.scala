package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.GraphPredef._

import vision.id.tessella.Tessella.Tiling

class gonalityTest extends FlatSpec with ConstraintUtils with Loggable {

  setLogLevel("WARN")

  val three: Tiling = Tiling.threeUniformOneOneOne8(6, 6)

  "A 3-uniform tessellation" can "have its pgon counted by type" in {
    assert(
      three.pgonsMap === Map(
        4 -> 18,
        3 -> 38,
        6 -> 12
      ))
  }

  it must "have gonality 3" in {
    assert(three.gonality === 3)
  }

  it can "NOT be monogonal" in {
    assert(three.isMonogonal === false)
  }

  "A square net" must "be monogonal" in {
    assert(Tiling.squareNet(3, 3).isMonogonal)
  }

  "A tessellation from a full vertex" must "be monogonal" in {
    assert(Full.s("(3.4.6.4)").toTiling.isMonogonal)
  }

  "A tessellation from a partial vertex" must "be monogonal" in {
    assert(Vertex.s("(3.4.6)").toTiling.isMonogonal)
  }

  "A tessellation without full vertices" can "be monogonal" in {
    assert((Tiling.squareNet(3, 2) --= Set(1, 12)).isMonogonal)
  }

  it can "also be NOT monogonal" in {
    assert((Tiling.twoUniform(2,1) -= Side(1, 2)).isMonogonal === false)
  }

}
