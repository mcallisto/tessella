package vision.id.tessella

import scala.util.{Failure, Success, Try}

trait TryUtils {

  /**
    * combine two tries
    *
    * @param a first try
    * @param b second try
    * @param f combinator
    * @tparam A type of first
    * @tparam B type of second
    * @tparam C type of resulting try
    * @return
    */
  def map2[A, B, C](a: Try[A], b: Try[B])(f: (A, B) => C): Try[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  /**
    * from list of tries to try of list
    *
    * @param a list of tries
    * @tparam A type
    * @return failure if any of the tries fails
    */
  def sequence[A](a: List[Try[A]]): Try[List[A]] =
    a.foldRight(Success(Nil): Try[List[A]])((x, y) => map2(x, y)(_ :: _))

  final implicit class Tr[T](y: Try[T]) {

    def safeGet: T = y match {
      case Success(t) => t
      case Failure(e) => throw new Exception(e)
    }

  }

}
