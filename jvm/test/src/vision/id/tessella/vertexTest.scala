package vision.id.tessella

import org.scalatest.FlatSpec

import vision.id.tessella.Tau.TAU

class vertexTest extends FlatSpec with MathUtils with TryUtils {

  val fourThree: Vertex = Vertex(List(Square, Triangle))

  "A vertex" can "be printed as a string" in {
    assert(fourThree.toString() === "(▲.■)")
  }

  it can "be created from a list of sides of reg p-gons" in {
    assert(Vertex.p(List()) === Vertex(List()))
    assert(Vertex.fromSides(List(4, 3)).safeGet === fourThree)
    assert(Vertex.p(List(4, 3)) === fourThree)
  }

  it can "be created from its string description" in {
    val hex = Vertex.p(List(3, 3, 3, 3, 3, 3))
    assert(Vertex.s("(■.▲)") === fourThree)
    assert(Vertex.s("(▲*6)") === hex)
    assert(Vertex.s("(▲⁶)") === hex)
    assert(Vertex.s("()") === Vertex(List()))
  }

  it must "have the interior angle not larger than 360°" in {
    assertThrows[Exception] {
      Vertex.s("(⬣⁴)")
    }
  }

  it can "be equal to another vertex" in {
    assert(Vertex.s("(■.▲)") === fourThree)
    assert(Vertex.s("(■.▲)").equals(fourThree))
  }

  it can "be a reflection of another vertex" in {
    assert(Vertex.s("(▲.■)") !== fourThree)
    assert(Vertex.s("(▲.■)").isReflectionOf(fourThree))
  }

  it can "be checked for a full interior angle" in {
    assert(Vertex.s("(▲⁶)").isFull)
  }

  it can "be listed in all different variants" in {
    assert(Vertex.s("(⬣³)").allVersions === List(Vertex.s("(⬣³)")))
    assert(
      Vertex.s("(⬣.■.▲.■)").allVersions === List(
        Vertex.s("(⬣.■.▲.■)"),
        Vertex.s("(■.▲.■.⬣)")
      ))
  }

  it can "be equivalent to another symmetric (reflected) vertex" in {
    assert(Vertex.s("(⬣.■.▲)").isEquivalentTo(Vertex.s("(⬣.■.▲)")) === true)
    assert(Vertex.s("(⬣.■.▲)").isEquivalentTo(Vertex.s("(▲.■.⬣)")) === true)
    assert(Vertex.s("(⬣.■.▲)").isEquivalentTo(Vertex.s("(■.⬣.▲)")) === false)
  }

  it can "be converted into cartesian points" in {
    assert(
      Vertex.s("(■)").toPoint2Ds().map(_.toString) ===
        List("{1.0:0.0}", "{0.0:1.0}"))
    assert(
      Vertex.s("(■)").toPoint2Ds(TAU / 4).map(_.toString) ===
        List("{0.0:1.0}", "{-1.0:0.0}"))
    assert(
      Vertex.s("(▲)").toPoint2Ds().map(_.toString) ===
        List("{1.0:0.0}", "{0.5:0.8660254}"))
    assert(
      Vertex.s("(⬣³)").toPoint2Ds().map(_.toString) ===
        List("{1.0:0.0}", "{-0.5:0.8660254}", "{-0.5:-0.8660254}"))
  }

  it can "be transformed into distinct" in {
    assert(Vertex.s("(▲².■.▲²)").distinct === Vertex.s("(▲.■)"))
  }

  "Vertices" can "be ordered" in {
    assert(
      List(
        Vertex.s("(⬣.■.▲.■)"),
        Vertex.s("(■.▲.■.⬣)"),
        Vertex.s("(■.⬣.■.▲)"),
        Vertex.s("(▲.■.⬣.■)")
      ).sorted === List(
        Vertex.s("(▲.■.⬣.■)"),
        Vertex.s("(■.▲.■.⬣)"),
        Vertex.s("(■.⬣.■.▲)"),
        Vertex.s("(⬣.■.▲.■)")
      ))
    assert(
      List(
        Vertex.s("(⬣.■.▲.■)"),
        Vertex.s("(⬣.■.▲)")
      ).sorted === List(
        Vertex.s("(⬣.■.▲)"),
        Vertex.s("(⬣.■.▲.■)")
      ))
    assert(
      List(
        Vertex.s("(⬣.■.▲)"),
        Vertex.s("(⬣.■.▲.■)")
      ).sorted === List(
        Vertex.s("(⬣.■.▲)"),
        Vertex.s("(⬣.■.▲.■)")
      ))
  }

}
