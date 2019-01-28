package vision.id.tessella

import org.scalatest.FlatSpec

class fullTest extends FlatSpec with MathUtils with ListUtils {

  "A full vertex" can "be listed in all different variants" in {
    assert(Full.s("(6*3)").allVersions === List(Full.s("(6*3)")))
    assert(
      Full.s("(6.4.3.4)").allVersions === List(
        Full.s("(6.4.3.4)"),
        Full.s("(4.3.4.6)"),
        Full.s("(4.6.4.3)"),
        Full.s("(3.4.6.4)")
      ))
  }

  it can "be equivalent to another symmetric (rotated or reflected) full vertex" in {
    assert(Full.s("(6.4.4.3)").isEquivalentTo(Full.s("(6.4.4.3)")) === true)
    assert(Full.s("(6.4.4.3)").isEquivalentTo(Full.s("(3.4.4.6)")) === true)
    assert(Full.s("(6.4.4.3)").isEquivalentTo(Full.s("(4.6.3.4)")) === true)
    assert(Full.s("(6.4.4.3)").isEquivalentTo(Full.s("(6.4.3.4)")) === false)
  }

  "Full vertices" can "be ordered" in {
    assert(
      List(
        Full.s("(6.4.3.4)"),
        Full.s("(4.3.4.6)"),
        Full.s("(4.6.4.3)"),
        Full.s("(3.4.6.4)")
      ).sorted(Vertex.orderingByRegPgon) === List(
        Full.s("(3.4.6.4)"),
        Full.s("(4.3.4.6)"),
        Full.s("(4.6.4.3)"),
        Full.s("(6.4.3.4)")
      ))
  }

}
