package vision.id.tessella

trait ListUtils extends OptionUtils with Symmetry {

  final implicit class Util[T](l: List[T]) {

    def circularSliding(size: Int, step: Int = 1): Iterator[List[T]] =
      (l ++ l.take(size - 1)).sliding(size, step)

    def groupWithIndex: Map[T, List[Int]] =
      l.zipWithIndex.groupBy({ case (elem, _) => elem }).mapValues(_.unzip match { case (_, indexes) => indexes })

    def indexesOf(elem: T, from: Int = 0): List[Int] = {

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

    def circularNeighborsOf(elem: T): Option[List[T]] = l.indexOf(elem) match {
      case -1                   => None
      case 0 if l.size == 1     => Some(Nil)
      case 0                    => Some(List(sLast(l), l(1)))
      case i if i == l.size - 1 => Some(List(l(i - 1), sHead(l)))
      case i                    => Some(List(l(i - 1), l(i + 1)))
    }

  }

  final implicit class Util2[T](ll: List[List[T]]) {

    def headLastConcat: List[List[T]] = {

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
