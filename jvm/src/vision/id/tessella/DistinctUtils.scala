package vision.id.tessella

import scala.annotation.tailrec

trait DistinctUtils[T] {

  final implicit class Dist(l: List[T]) {

    /**
      * distinct elements by function
      *
      * @param f distinction function
      * @return
      */
    def distinctBy(f: (T, T) => Boolean): List[T] = l match {
      case Nil => Nil
      case h :: t =>
        @tailrec
        def loop(ll: List[T], acc: List[T]): List[T] = ll match {
          case Nil => acc.reverse
          case hh :: tt =>
            val newAcc = if (acc.exists(f(_, hh))) acc else hh +: acc
            loop(tt, newAcc)
        }

        loop(t, List(h))
    }

    /**
      * indexes of distinct elements by function
      *
      * @param f distinction function
      * @return
      */
    def distinctByIndexes(f: (T, T) => Boolean): List[Int] = l match {
      case Nil => Nil
      case h :: t =>
        @tailrec
        def loop(ll: List[T], i: Int, acc: List[(Int, T)]): List[Int] =
          ll match {
            case Nil => acc.map({ case (index, _) => index }).reverse
            case hh :: tt =>
              val newAcc =
                if (acc.exists({ case (_, elem) => f(elem, hh) })) acc
                else (i, hh) +: acc
              loop(tt, i + 1, newAcc)
          }

        loop(t, 1, List((0, h)))
    }

  }

}
