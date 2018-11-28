package vision.id.tessella

import org.scalatest.FlatSpec

class symmetryTest extends FlatSpec with Symmetry {

  val stringList: List[String] = List("a", "b", "c")

  "A list" can "be rotated 1 step to the right" in {
    assert(stringList.rotate(1) === List("c", "a", "b"))
  }

  it can "be rotated 1 step to the left" in {
    assert(stringList.rotate(-1) === List("b", "c", "a"))
  }

  it can "be rotated 2 steps to the left" in {
    assert(stringList.rotate(-2) === List("c", "a", "b"))
  }

  it can "be rotated 5 steps to the left" in {
    assert(stringList.rotate(-5) === List("c", "a", "b"))
  }

  it can "be rotated 6 steps to the left" in {
    assert(stringList.rotate(-6) === stringList)
  }

  it can "be rotated 0 steps" in {
    assert(stringList.rotate(0) === stringList)
  }

  it can "generate all rotations" in {
    assert(
      stringList.rotations === List(
        List("a", "b", "c"),
        List("c", "a", "b"),
        List("b", "c", "a")
      ))
  }

  it can "be reflected once" in {
    assert(stringList.reflect() === List("c", "b", "a"))
  }

  it can "be reflected twice" in {
    assert(stringList.reflect(2) === stringList)
  }

  it can "be reflected 4 times" in {
    assert(stringList.reflect(4) === stringList)
  }

  it can "generate all (two) reflections" in {
    assert(
      stringList.reflections === List(
        List("a", "b", "c"),
        List("c", "b", "a")
      ))
  }

  it can "generate all rotareflections" in {
    assert(
      stringList.rotaReflections === List(
        List("a", "b", "c"),
        List("c", "b", "a"),
        List("c", "a", "b"),
        List("b", "a", "c"),
        List("b", "c", "a"),
        List("a", "c", "b")
      ))
  }

  it can "be counted for the number of rotareflection axis" in {
    assert(List("a", "b", "a", "b", "a", "b").rotaReflectionCount === 6)
    assert(List().rotaReflectionCount === 2)
    assert(List("a").rotaReflectionCount === 2)
  }

  it can "be contrarotated" in {
    assert(stringList.contraRotate() === List("a", "c", "b"))
    assert(stringList.contraRotate(0) === List("c", "b", "a"))
  }

  it can "be rotated or contrarotated" in {
    assert(stringList.rotateWithDirection(1) === List("c", "a", "b"))
    assert(stringList.rotateWithDirection(1, dir = false) === List("c", "b", "a"))
  }

  "Two lists" can "be one a rotation of the other" in {
    assert(List("b", "c").isRotationOf(stringList) === false)
    assert(List("b", "c", "a").isRotationOf(stringList) === true)
  }

  they can "be one a reflection of the other" in {
    assert(List("c", "b", "a").isReflectionOf(stringList) === true)
  }

  they can "be one a rotation or a reflection of the other" in {
    assert(List("c", "b", "a").isRotationOrReflectionOf(stringList) === true)
  }

}
