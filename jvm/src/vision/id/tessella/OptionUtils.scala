package vision.id.tessella

trait OptionUtils {

  final implicit class Opt[T](o: Option[T]) {

    def safeGet(msg: String = "cannot get, none"): T = o match {
      case Some(t) => t
      case None    => throw new IllegalArgumentException(msg)
    }

  }

}
