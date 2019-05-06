package vision.id.tessella

import scala.annotation.tailrec

trait ListUtils extends OptionUtils with Symmetry {

  final implicit class Util[T](l: List[T]) {

    def circularSliding(size: Int, step: Int = 1): Iterator[List[T]] =
      (l ++ l.take(size - 1)).sliding(size, step)

    def groupWithIndex: Map[T, List[Int]] =
      l.zipWithIndex.groupBy({ case (elem, _) => elem }).mapValues(_.unzip match { case (_, indexes) => indexes })

    def indexesOf(elem: T, from: Int = 0): List[Int] = {

      @tailrec
      def loop(f: Int, acc: List[Int]): List[Int] = l.indexOf(elem, f) match {
        case -1 => acc
        case i  => loop(i + 1, i +: acc)
      }

      loop(from, List()).reverse
    }

    def indexesOfWithReflection(elem: List[T], from: Int = 0): List[Int] =
      l.sliding(elem.size)
        .flatMap(_.reflections)
        .toList
        .indexesOf(elem, from * 2)
        .map(_ / 2)
        .distinct

    def safeHead: T = sHead(l)

    def safeLast: T = sLast(l)

    def circularNeighborsOf(elem: T): Option[(T, T)] = l.indexOf(elem) match {
      case -1                   => None
      case 0 if l.size == 1     => Some((elem, elem))
      case 0                    => Some((sLast(l), l(1)))
      case i if i == l.size - 1 => Some((l(i - 1), sHead(l)))
      case i                    => Some((l(i - 1), l(i + 1)))
    }

    def circularPathsIndexesExcluded(first: Int, second: Int): List[List[T]] = {
      val (a1, temp) = l.splitAt(first + 1)
      val (b, a2)    = temp.splitAt(second - (first + 1))
      List(a1.init ++ a2.tail, b)
    }

    def hasOnlySameElement(f: (T, T) => Boolean = _ == _): Boolean = l match {
      case elem :: elems => elems.forall(f(_, elem))
      case Nil           => false
    }

    def onlyTwoElements[U](f: (T, T) => U): U = l match {
      case first :: second :: Nil => f(first, second)
      case _                      => throw new Error
    }
  }

  final implicit class Util2[T](ll: List[List[T]]) {

    def headLastConcat: List[List[T]] = {

      @tailrec
      def loop(ts: List[List[T]], acc: List[List[T]]): List[List[T]] = ts match {
        case Nil    => acc
        case h :: t => loop(t, if (sHead(h) == sLast(sHead(acc))) h +: acc else h.reverse +: acc)
      }

      loop(ll.tail, List(sHead(ll))).reverse

    }
  }

  private def sHead[T](l: List[T]): T = l.headOption.safeGet("no head, empty list")

  private def sLast[T](l: List[T]): T = l.lastOption.safeGet("no last, empty list")

}
