package vision.id.tessella

import org.scalatest.FlatSpec

import vision.id.tessella.Others.Mono
import vision.id.tessella.Tessella.Tiling

import scala.util.Try

class monoAddTest extends FlatSpec with AddUtils with Loggable {

  setLogLevel("WARN")

  // ---------------- adding single node ----------------

  def eightSquares: Mono = {
    val t = Tiling.squareNet(3, 3)
    t -= Side(2, 3)
    Mono.fromTiling(t)
  }

  "A mono of eight squares with a full (4*4) vertex" can "have an edge added" in {
    val m = eightSquares
    assert(Try(m += Side(2, 3)).isSuccess)
  }

  it can "have edges forming another square added" in {
    val m = eightSquares
    assert(Try(m ++= Set(Side(1, 17), Side(17, 18), Side(18, 2))).isSuccess)
  }

  "A mono of four hexs with a full (6*3) vertex" can "have edges forming another hex added" in {
    val m = Mono.fromTiling(Tiling.hexagonNet(2, 2))
    assert(Try(m ++= Set(Side(2, 17), Side(17, 18), Side(18, 19), Side(19, 4))).isSuccess)
  }

  "A mono of twelve triangles with a full (3*6) vertex" can "NOT have edges forming an hex added" in {
    val m = Mono.fromTiling(Tiling.threeUniformTwoOne(2, 1))
    assert(Try(m ++= Set(Side(2, 13), Side(13, 14), Side(14, 15), Side(15, 4))).isFailure)
  }

}
