package vision.id.tessella

trait ListUtils extends Symmetry {

  final implicit class Util[T](l: List[T]) {

    def circularSliding(size: Int, step: Int = 1): Iterator[List[T]] =
      (l ++ l.take(size - 1)).sliding(size, step)

    def groupWithIndex: Map[T, List[Int]] =
      l.zipWithIndex.groupBy(_._1).mapValues(_.unzip._2)

    def indexesOf(elem: T, from: Int = 0): List[Int] = {

      def loop(f: Int, acc: List[Int]): List[Int] = l.indexOf(elem, f) match {
        case -1 ⇒ acc
        case i  ⇒ loop(i + 1, i +: acc)
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

  }

  final implicit class Util2[T](ll: List[List[T]]) {

    def headLastConcat: List[List[T]] = {

      def loop(ts: List[List[T]], acc: List[List[T]]): List[List[T]] = ts match {
        case Nil    ⇒ acc
        case h :: t ⇒ loop(t, if (h.head == acc.head.last) h +: acc else h.reverse +: acc)
      }

      loop(ll.tail, List(ll.head)).reverse

    }
  }

}
