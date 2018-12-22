package vision.id.tessella

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class symmetryCheckTest extends FlatSpec with Checkers with Symmetry {

  val alphabet: List[Char] = List('a', 'b', 'c', 'd', 'e', 'F', 'G', 'H', 'I')

  "A list containing just the same element" must "be equal to itself when reflected an arbitrary number of times" in {
    val sameReflectionListGen: Gen[(List[Char], List[Char])] = for {
      l     <- Gen.containerOf[List, Char](Gen.const(alphabet.head))
      steps <- Gen.choose(-999, 999)
    } yield (l, l.reflect(steps))

    check(forAll(sameReflectionListGen) { case (original, reflection) => original == reflection })
  }

  it must "be equal to itself when rotated by an arbitrary number of steps" in {
    val sameRotationListGen: Gen[(List[Char], List[Char])] = for {
      l     <- Gen.containerOf[List, Char](Gen.const(alphabet.head))
      steps <- Gen.choose(-999, 999)
    } yield (l, l.rotate(steps))

    check(forAll(sameRotationListGen) { case (original, rotation) => original == rotation })
  }

  "A list" must "be equal to itself when reflected an even number of times" in {
    val evenReflectionListGen: Gen[(List[Char], List[Char])] = for {
      l     <- Gen.containerOf[List, Char](Gen.oneOf(alphabet))
      steps <- Gen.choose(-999, 999) suchThat (_ % 2 == 0)
    } yield (l, l.reflect(steps))

    check(forAll(evenReflectionListGen) { case (original, reflection) => original == reflection })
  }

  it must "be equal to its single reflection when reflected an odd number of times" in {
    val oddReflectionListGen: Gen[(List[Char], List[Char])] = for {
      l     <- Gen.containerOf[List, Char](Gen.oneOf(alphabet))
      steps <- Gen.choose(-999, 999) suchThat (_ % 2 == 1)
    } yield (l, l.reflect(steps))

    check(forAll(oddReflectionListGen) { case (original, reflection) => original.reflect() == reflection })
  }

  it must "be equal to itself when rotated by full circles" in {
    val clockRotationListGen: Gen[(List[Char], List[Char])] = for {
      l     <- Gen.containerOf[List, Char](Gen.oneOf(alphabet))
      steps <- Gen.choose(-9, 9)
    } yield (l, l.rotate(steps * l.size))

    check(forAll(clockRotationListGen) { case (original, rotation) => original == rotation })
  }

}
