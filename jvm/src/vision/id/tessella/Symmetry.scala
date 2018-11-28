package vision.id.tessella

/**
  * symmetry on generic lists viewed as cyclic
  *
  * @define ELEMTYPE type of elements listed
  * @author Mario Càllisto
  */
trait Symmetry {

  /**
    * @param action action to be performed on list
    * @param times  number of symmetries, that is times the action will be performed
    * @tparam T $ELEMTYPE
    * @return
    */
  private def results[T](l: List[T], action: (List[T], Int) ⇒ List[T], times: Int): IndexedSeq[List[T]] =
    l +: (1 until times).map(action(l, _))

  /**
    * @param other candidate symmetric list
    * @param f     symmetry results of other
    * @tparam T $ELEMTYPE
    * @return
    */
  private def check[T](l: List[T], other: List[T], f: List[T] ⇒ IndexedSeq[List[T]]): Boolean =
    l.lengthCompare(other.size) == 0 && f(other).distinct.contains(l)

  /**
    * @param l list
    * @tparam T $ELEMTYPE
    */
  implicit final class Symm[T](l: List[T]) {

    def reflect(steps: Int = 1): List[T] =
      (0 until steps).foldLeft(l)({ case (m, _) ⇒ m.reverse })

    def reflections: IndexedSeq[List[T]] =
      results[T](l, _.reflect(_), 2)

    def isReflectionOf(other: List[T]): Boolean =
      check[T](l, other, _.reflections)

    /**
      * shift lists of number of steps
      *
      * @param steps positive move to the right
      * @return
      */
    def rotate(steps: Int): List[T] = l.size match {
      case 0 ⇒ l
      case 1 ⇒ l
      case size ⇒
        val s = steps.abs % size
        if (steps < 0)
          l.drop(s) ++ l.take(s)
        else
          l.takeRight(s) ++ l.dropRight(s)
    }

    def rotations: IndexedSeq[List[T]] =
      results[T](l, _.rotate(_), l.size)

    def isRotationOf(other: List[T]): Boolean =
      check[T](l, other, _.rotations)

    def rotaReflections: IndexedSeq[List[T]] =
      l.rotations.flatMap(_.reflections)

    def isRotationOrReflectionOf(other: List[T]): Boolean =
      check[T](l, other, _.rotaReflections)

    /**
      * @param steps default -1 means keeping the same head but elements flowing in the opposite direction
      * @return
      */
    def contraRotate(steps: Int = -1): List[T] =
      l.rotate(steps).reflect()

    def rotateWithDirection(steps: Int, dir: Boolean = true): List[T] =
      if (dir) l.rotate(steps) else l.contraRotate(steps - 1)

    def rotaReflectionCount: Int =
      l.rotaReflections.count(_ == l)

  }

}
