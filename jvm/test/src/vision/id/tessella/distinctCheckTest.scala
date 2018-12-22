package vision.id.tessella

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class distinctCheckTest extends FlatSpec with Checkers with DistinctUtils[Char] {

  val alphabet: List[Char] = List('a', 'b', 'c', 'd', 'e', 'F', 'G', 'H', 'I')

  val equality: (Char, Char) => Boolean = _ == _

  "A list composed by only distinct elements" can "NOT be reduced" in {
    val distinctListGen: Gen[List[Char]] = for {
      variety <- Gen.choose(0, 9)
    } yield alphabet.take(variety)

    check(forAll(distinctListGen)(l => l.distinctBy(equality) == l))
  }

  val varietyListGen: Gen[(Int, List[Char])] = for {
    variety <- Gen.choose(0, 9)
    letters = alphabet.take(variety)
    l <- if (variety == 0) Gen.const(List(): List[Char])
    else Gen.containerOf[List, Char](Gen.oneOf(letters))
  } yield (variety, l ++ letters)

  "A list composed by n distinct elements" must "be reduced to n size" in {
    check(forAll(varietyListGen) { case (variety, l) => l.distinctBy(equality).lengthCompare(variety) == 0 })
  }

  val indistinct: (Char, Char) => Boolean = (_, _) => true

  "A list composed by only indistinct elements" can "be empty or with size 1" in {
    check(forAll(varietyListGen) {
      case (variety, l) =>
        val d = l.distinctBy(indistinct)
        variety match {
          case 0 => d.isEmpty
          case _ => d.lengthCompare(1) == 0
        }
    })
  }

  val indistinctLowercase: (Char, Char) => Boolean = (_, c) => c.isLower

  "A list composed by indistinct and distinct elements" can "be empty, with size 1 or more" in {
    check(forAll(varietyListGen) {
      case (variety, l) =>
        val d = l.distinctBy(indistinctLowercase)
        variety match {
          case 0                  => d.isEmpty
          case lower if lower < 6 => d.lengthCompare(1) == 0
          case _                  => d.lengthCompare(1) > 0
        }
    })
  }

  "Indexes of found distinct elements" must "identify the distinct elements" in {
    check(forAll(varietyListGen) {
      case (_, l) =>
        l.distinctBy(equality) == l.distinctByIndexes(equality).map(l(_)) &&
          l.distinctBy(indistinct) == l.distinctByIndexes(indistinct).map(l(_)) &&
          l.distinctBy(indistinctLowercase) == l.distinctByIndexes(indistinctLowercase).map(l(_))
    })
  }

}
