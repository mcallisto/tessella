package vision.id.tessella.creation

import scala.annotation.tailrec
import scala.util.Try

import vision.id.tessella._
import vision.id.tessella.Others.Mono

/**
  * slow methods to create tessellations with "organic" growth
  */
trait Growth extends AddUtils {

  /** always search for smaller pgons first, except special case
    *
    * @todo this has to be reviewed considering uniformity
    * @param pattern full vertex
    * @return
    */
  private def orderedPgons(pattern: Full): List[RegPgon] = {
    val pgons = pattern.distinct.pgons
    if (pattern == Full.s("(3*4.6)")) pgons.reverse else pgons
  }

  /**
    * grow tessell of given size by respecting only 1 pattern
    *
    * @param pattern full vertex
    * @param size    new size in p-gons
    * @return
    */
  def expandPattern(pattern: Full, size: Int = 100): Try[Mono] = Try {
    pattern.pgons.size match {
      case less if size <= less => Mono.fromTiling(Vertex(pattern.pgons.take(size)).toTiling)
      case n =>
        val m = Mono.fromTiling(pattern.toTiling)
        if (pattern.isRegular)
          (n until size).foreach(_ => m.addToEdge(m.perimeterMinEdge, pattern.pgons.safeHead))
        else {
          val ps = orderedPgons(pattern)

          @tailrec
          def loop(edges: List[Side[Int]], count: Int): Unit =
            if (count == size) ()
            else
              edges match {
                case Nil => throw new IllegalArgumentException
                case e :: es =>
                  if (ps.exists(p => m.addToEdge(e, p).isSuccess))
                    loop(m.perimeterEdgesByMin, count + 1)
                  else
                    loop(es, count)
              }

          loop(m.perimeterEdgesByMin, n)
        }
        m
    }
  }

  /**
    * all tessellations from 1 to given size grown by respecting only 1 pattern
    *
    * @param pattern full vertex
    * @param size    new size in p-gons
    * @return
    */
  def scanPattern(pattern: Full, size: Int = 100): List[Try[Mono]] = pattern.pgons.size match {
    case less if size <= less => Vertex(pattern.pgons.take(size)).toScan.map(t => Try(Mono.fromTiling(t)))
    case n =>
      val start = Vertex(pattern.pgons).toScan.map(t => Try(Mono.fromTiling(t)))
      val m     = Mono.fromTiling(Vertex(pattern.pgons).toTiling)
      if (pattern.isRegular)
        (n until size)
          .foldLeft(start.reverse)((l, _) => {
            m.addToEdge(m.perimeterMinEdge, pattern.pgons.safeHead).map(_ => m.clone) +: l
          })
          .reverse
      else {
        val ps = orderedPgons(pattern)

        @tailrec
        def loop(edges: List[Side[Int]], count: Int, acc: List[Try[Mono]]): List[Try[Mono]] =
          if (count == size) acc
          else
            edges match {
              case Nil => Try(throw new IllegalArgumentException) +: acc
              case e :: es =>
                if (ps.exists(p => m.addToEdge(e, p).isSuccess))
                  loop(m.perimeterEdgesByMin, count + 1, Try(m.clone) +: acc)
                else
                  loop(es, count, acc)
            }

        loop(m.perimeterEdgesByMin, n, start.reverse).reverse
      }
  }

}
