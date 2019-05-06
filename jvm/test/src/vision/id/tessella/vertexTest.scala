package vision.id.tessella

import org.scalatest.FlatSpec

import vision.id.tessella.Tau.TAU

class vertexTest extends FlatSpec with MathUtils with TryUtils {

  val fourThree: Vertex = Vertex(List(Square, Triangle))

  "A vertex" can "be printed as a string" in {
    assert(fourThree.toString === "(▲.■)")
  }

  it can "be empty" in {
    val empty = new Vertex(Nil)
    assert(empty.toString === "()")
  }

  it can "be created from a list of sides of reg p-gons" in {
    assert(Vertex.p(List()) === Vertex(List()))
    assert(Vertex.fromEdgesNumbers(List(4, 3)).safeGet === fourThree)
    assert(Vertex.p(List(4, 3)) === fourThree)
  }

  it can "be created from its string description" in {
    val hex = Vertex.p(List(3, 3, 3, 3, 3, 3))
    assert(Vertex.s("(■.▲)") === fourThree)
    assert(Vertex.s("(▲*6)") === hex)
    assert(Vertex.s("(3⁶)") === hex)
    assert(Vertex.s("()") === Vertex(List()))
  }

  it must "have the interior angle not larger than 360°" in {
    assertThrows[Exception] {
      Vertex.s("(6⁴)")
    }
  }

  it can "be equal to another vertex" in {
    assert(Vertex.s("(4.3)") === fourThree)
    assert(Vertex.s("(4.3)").equals(fourThree))
  }

  it can "be a reflection of another vertex" in {
    assert(Vertex.s("(3.4)") !== fourThree)
    assert(Vertex.s("(3.4)").isReflectionOf(fourThree))
  }

  it can "be checked for a full interior angle" in {
    assert(Vertex.s("(3⁶)").isFull)
  }

  it can "be listed in all different variants" in {
    assert(Vertex.s("(6³)").allVersions === List(Vertex.s("(6³)")))
    assert(
      Vertex.s("(6.4.3.4)").allVersions === List(
        Vertex.s("(6.4.3.4)"),
        Vertex.s("(4.3.4.6)")
      ))
  }

  it can "be equivalent to another symmetric (reflected) vertex" in {
    assert(Vertex.s("(6.4.3)").isEquivalentTo(Vertex.s("(6.4.3)")) === true)
    assert(Vertex.s("(6.4.3)").isEquivalentTo(Vertex.s("(3.4.6)")) === true)
    assert(Vertex.s("(6.4.3)").isEquivalentTo(Vertex.s("(4.6.3)")) === false)
  }

  it can "be converted into cartesian points" in {
    assert(
      Vertex.s("(4)").toPoint2Ds().map(_.toString) ===
        List("{1.0:0.0}", "{0.0:1.0}"))
    assert(
      Vertex.s("(4)").toPoint2Ds(TAU / 4).map(_.toString) ===
        List("{0.0:1.0}", "{-1.0:0.0}"))
    assert(
      Vertex.s("(3)").toPoint2Ds().map(_.toString) ===
        List("{1.0:0.0}", "{0.5:0.8660254}"))
    assert(
      Vertex.s("(6³)").toPoint2Ds().map(_.toString) ===
        List("{1.0:0.0}", "{-0.5:0.8660254}", "{-0.5:-0.8660254}"))
  }

  it can "be transformed into distinct" in {
    assert(Vertex.s("(3².4.3²)").distinct === Vertex.s("(3.4)"))
  }

  "Vertices" can "be ordered" in {
    assert(
      List(
        Vertex.s("(6.4.3.4)"),
        Vertex.s("(4.3.4.6)"),
        Vertex.s("(4.6.4.3)"),
        Vertex.s("(3.4.6.4)")
      ).sorted === List(
        Vertex.s("(3.4.6.4)"),
        Vertex.s("(4.3.4.6)"),
        Vertex.s("(4.6.4.3)"),
        Vertex.s("(6.4.3.4)")
      ))
    assert(
      List(
        Vertex.s("(6.4.3.4)"),
        Vertex.s("(6.4.3)")
      ).sorted === List(
        Vertex.s("(6.4.3)"),
        Vertex.s("(6.4.3.4)")
      ))
    assert(
      List(
        Vertex.s("(6.4.3)"),
        Vertex.s("(6.4.3.4)")
      ).sorted === List(
        Vertex.s("(6.4.3)"),
        Vertex.s("(6.4.3.4)")
      ))
  }


  "Vertex (4)" must "be contained in vertex (3.4.6)" in {
    assert(Vertex.s("(4)").isContainedIn(Vertex.s("(3.4.6)")))
  }

  "Vertex (3.4)" must "be contained in vertex (4.4.3)" in {
    assert(Vertex.s("(3.4)").isContainedIn(Vertex.s("(4.4.3)")))
  }

  "Vertex (4.3)" must "be contained in vertex (4.4.3)" in {
    assert(fourThree.isContainedIn(Vertex.s("(4.4.3)")))
  }

  it must "be contained in vertex (6.4.3.4)" in {
    assert(fourThree.isContainedIn(Vertex.s("(6.4.3.4)")))
  }

  it can "be merged with vertex (6.4)" in {
    assert(fourThree.merge(Vertex.s("(6.4)")) === List(Vertex.s("(3.4.6)")))
  }

  it can "be merged with vertex (4.4)" in {
    assert(fourThree.merge(Vertex.s("(4.4)")) === List(Vertex.s("(3.4.4)")))
  }

  it can "be merged with vertex (4.6.4)" in {
    assert(fourThree.merge(Vertex.s("(4.6.4)")) === List(Vertex.s("(3.4.6.4)")))
  }

  it can "be merged with vertex (3.6.4)" in {
    assert(fourThree.merge(Vertex.s("(3.6.4)")) === List(Vertex.s("(3.4.4.6)"), Vertex.s("(3.4.6.3)")))
  }

  it can "NOT be merged with vertex (4.4.8)" in {
    assert(fourThree.merge(Vertex.s("(4.4.8)")).isEmpty)
  }

  "Vertex (4.4)" can "be merged with vertex (4.4)" in {
    assert(Vertex.s("(4.4)").merge(Vertex.s("(4.4)")) === List(Vertex.s("(4.4)")))
  }

}
