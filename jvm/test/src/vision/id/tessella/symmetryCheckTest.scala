package vision.id.tessella

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class symmetryCheckTest extends FlatSpec with Checkers with Symmetry {

  val alphabet: List[Char] = List('a', 'b', 'c', 'd', 'e', 'F', 'G', 'H', 'I')

  "A list containing just the same element" must "be equal to itself when reflected an arbitrary number of times" in {
    val SameReflectionListGen: Gen[(List[Char], List[Char])] = for {
      l     ← Gen.containerOf[List, Char](Gen.const(alphabet.head))
      steps ← Gen.choose(-999, 999)
    } yield (l, l.reflect(steps))

    check(forAll(SameReflectionListGen) { case (original, reflection) ⇒ original == reflection })
  }

  it must "be equal to itself when rotated by an arbitrary number of steps" in {
    val SameRotationListGen: Gen[(List[Char], List[Char])] = for {
      l     ← Gen.containerOf[List, Char](Gen.const(alphabet.head))
      steps ← Gen.choose(-999, 999)
    } yield (l, l.rotate(steps))

    check(forAll(SameRotationListGen) { case (original, rotation) ⇒ original == rotation })
  }

  "A list" must "be equal to itself when reflected an even number of times" in {
    val EvenReflectionListGen: Gen[(List[Char], List[Char])] = for {
      l     ← Gen.containerOf[List, Char](Gen.oneOf(alphabet))
      steps ← Gen.choose(-999, 999) suchThat (_ % 2 == 0)
    } yield (l, l.reflect(steps))

    check(forAll(EvenReflectionListGen) { case (original, reflection) ⇒ original == reflection })
  }

  it must "be equal to its single reflection when reflected an odd number of times" in {
    val OddReflectionListGen: Gen[(List[Char], List[Char])] = for {
      l     ← Gen.containerOf[List, Char](Gen.oneOf(alphabet))
      steps ← Gen.choose(-999, 999) suchThat (_ % 2 == 1)
    } yield (l, l.reflect(steps))

    check(forAll(OddReflectionListGen) { case (original, reflection) ⇒ original.reflect() == reflection })
  }

  it must "be equal to itself when rotated by full circles" in {
    val ClockRotationListGen: Gen[(List[Char], List[Char])] = for {
      l     ← Gen.containerOf[List, Char](Gen.oneOf(alphabet))
      steps ← Gen.choose(-9, 9)
    } yield (l, l.rotate(steps * l.size))

    check(forAll(ClockRotationListGen) { case (original, rotation) ⇒ original == rotation })
  }

}
