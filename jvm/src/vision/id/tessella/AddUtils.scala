package vision.id.tessella

trait AddUtils {

  /**
    * get new ids for p-gons to be added
    *
    * @param n          number of new ids needed
    * @param emptiesMax max id and unused ones in between
    * @return
    */
  def getFreeIds(n: Int, emptiesMax: (List[Int], Int)): List[Int] = n match {
    case e if e < 0 => throw new Error
    case 0          => List()
    case _ =>
      val (es, max) = emptiesMax
      val s         = es.size
      if (s >= n) es.take(n)
      else es ++ ((max + 1) to (max + n - s)).toList
  }

}
