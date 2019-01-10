package vision.id.tessella

import org.scalatest.FlatSpec

class listUtilsTest extends FlatSpec with ListUtils {

  "A list" can "have the sliding method used in a circular way" in {
    val l = List(1, 2, 3, 4, 5)
    assert(l.sliding(2).toList === List(List(1, 2), List(2, 3), List(3, 4), List(4, 5)))
    assert(l.circularSliding(2).toList === List(List(1, 2), List(2, 3), List(3, 4), List(4, 5), List(5, 1)))
    assert(l.sliding(2, 2).toList === List(List(1, 2), List(3, 4), List(5)))
    assert(l.circularSliding(2, 2).toList === List(List(1, 2), List(3, 4), List(5, 1)))
    assert(List(1, 2, 3).circularSliding(2).toList === List(List(1, 2), List(2, 3), List(3, 1)))
    assert(
      List(1, 2, 3, 4).circularSliding(4).toList ===
        List(List(1, 2, 3, 4), List(2, 3, 4, 1), List(3, 4, 1, 2), List(4, 1, 2, 3)))
  }

  it can "be searched for all positions of an element" in {
    val l = List(1, 2, 3, 2, 2, 4, 6)
    assert(l.indexesOf(2) === List(1, 3, 4))
    assert(l.indexesOf(2, 2) === List(3, 4))
  }

  it can "have a sublist (also reflected) searched for all positions" in {
    val l = List(1, 2, 3, 4, 3, 4, 3, 3, 3, 6, 6, 7)
    assert(l.indexesOfWithReflection(List(3, 4)) === List(2, 3, 4, 5))
    assert(l.indexesOfWithReflection(List(3, 3)) === List(6, 7))
  }

  it can "be searched for the circular neighbors of an element" in {
    val l = List('a', 'b', 'c', 'd', 'e')
    assert(l.circularNeighborsOf('f').isEmpty)
    assert(List(1).circularNeighborsOf(1) === Some(Nil))
    assert(l.circularNeighborsOf('a') === Some(List('e', 'b')))
    assert(l.circularNeighborsOf('e') === Some(List('d', 'a')))
    assert(l.circularNeighborsOf('c') === Some(List('b', 'd')))
  }

  "A list of lists" can "be concatenated from last to head" in {
    val ll = List(List(1, 2, 3), List(4, 3), List(4, 5, 6, 7, 8), List(9, 10, 8))
    assert(ll.headLastConcat === List(List(1, 2, 3), List(3, 4), List(4, 5, 6, 7, 8), List(8, 10, 9)))
  }

}
