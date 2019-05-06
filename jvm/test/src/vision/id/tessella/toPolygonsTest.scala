package vision.id.tessella

import org.scalatest.FlatSpec

import vision.id.tessella.Others.Mono

class toPolygonsTest extends FlatSpec with TilingUtils with Loggable {

  setLogLevel("WARN")

  "Mono rough case" can "be converted to polygons" in {
    val m: Mono = Mono.expandPattern(Full.s("(3.6.3.6)"), 8).safeGet
    assert(m.toPolygons(m.toNodesMap).size === 8)
  }

}
