package vision.id.tessella

import org.scalatest.FlatSpec

import vision.id.tessella.Tessella.Tiling

class additionTest extends FlatSpec with AddUtils with OptionUtils with TryUtils {

  val uShape: Tiling = Tiling.squareNet(3, 2) -= Side(2, 3)

  "A tessellation" can "have a polygon added replacing three perimeter edges" in {
    assert(uShape.addToEdge(Side(6, 7), Square).isSuccess)
    assert(uShape.edges.toString ===
      "EdgeSet(9-10, 1-2, 1-5, 5-9, 5=6, 2=6, 2-3, 6=7, 6=10, 3-4, 3=7, 10-11, 7=11, 7=8, 4-8, 11-12, 8-12)")
    assert(uShape.perimeterOrderedNodes === List(1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 1))
  }

  it can "have then another polygon added replacing one perimeter edge" in {
    assert(uShape.addToEdge(Side(1, 5), Square).isSuccess)
    assert(uShape.edges.toString ===
      "EdgeSet(9-10, 1-2, 1=5, 2=6, 2-3, 3-4, 3=7, 10-11, 4-8, 11-12, 13-14, 5-9, 5=6, 5-13, 6=7, 6=10, 7=11, 7=8, 14-1, 8-12)")
    assert(uShape.perimeterOrderedNodes === List(1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 13, 14, 1))
  }

}
