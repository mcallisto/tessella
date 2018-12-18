package vision.id.tessella

import org.scalatest.FlatSpec

class fullTest extends FlatSpec with MathUtils with ListUtils {

  "A full vertex" can "be listed in all different variants" in {
    assert(Full.s("(⬣³)").allVersions === List(Full.s("(⬣³)")))
    assert(
      Full.s("(⬣.■.▲.■)").allVersions === List(
        Full.s("(⬣.■.▲.■)"),
        Full.s("(■.▲.■.⬣)"),
        Full.s("(■.⬣.■.▲)"),
        Full.s("(▲.■.⬣.■)")
      ))
  }

  it can "be equivalent to another symmetric (rotated or reflected) full vertex" in {
    assert(Full.s("(⬣.■.■.▲)").isEquivalentTo(Full.s("(⬣.■.■.▲)")) === true)
    assert(Full.s("(⬣.■.■.▲)").isEquivalentTo(Full.s("(▲.■.■.⬣)")) === true)
    assert(Full.s("(⬣.■.■.▲)").isEquivalentTo(Full.s("(■.⬣.▲.■)")) === true)
    assert(Full.s("(⬣.■.■.▲)").isEquivalentTo(Full.s("(⬣.■.▲.■)")) === false)
  }

  "Full vertices" can "be ordered" in {
    assert(
      List(
        Full.s("(⬣.■.▲.■)"),
        Full.s("(■.▲.■.⬣)"),
        Full.s("(■.⬣.■.▲)"),
        Full.s("(▲.■.⬣.■)")
      ).sorted(Vertex.orderingByRegPgon) === List(
        Full.s("(▲.■.⬣.■)"),
        Full.s("(■.▲.■.⬣)"),
        Full.s("(■.⬣.■.▲)"),
        Full.s("(⬣.■.▲.■)")
      ))
  }

}
