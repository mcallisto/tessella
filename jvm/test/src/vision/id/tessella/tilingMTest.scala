package vision.id.tessella

import org.scalatest.FlatSpec
import scalax.collection.GraphPredef._

import vision.id.tessella.Tessella.TilingM

class tilingMTest extends FlatSpec with TilingUtils {

  val sq: TilingM = Square.toTilingM

  "A mutable tiling" can "be creates" in {
    assert(sq.edges.toString === "EdgeSet(1-2, 2-3, 3-4, 4-1)")
  }

  it can "be expanded" in {
    sq ++= Set(Side(1, 5), Side(5, 4))
    assert(sq.edges.toString === "EdgeSet(1-2, 1-5, 5-4, 2-3, 3-4, 4=1)")
  }

  it can "be reduced" in {
    sq -= Side(1, 4)
    assert(sq === Pentagon.toTilingM)
  }

}
