package vision.id.tessella.creation

import scala.util.Try

import vision.id.tessella._
import vision.id.tessella.Tessella.Tiling

/**
  * slow methods to create tessellations with "organic" growth
  */
trait Growth extends AddUtils {

  private def regPgonTiling(edgesNumber: Int): Try[Tiling] = RegPgon.ofEdges(edgesNumber).map(_.toTiling)

  /**
    * grow tessellation from vertex
    *
    * @param v vertex
    * @return
    */
  def fromVertex(v: Vertex): Tiling = v.edgesNumbers match {
    case edgesNumber :: numbers =>
      val t: Tiling = regPgonTiling(edgesNumber).safeGet
      numbers.foldLeft(edgesNumber)({
        case (count, number) =>
          t.addToEdgePgon2(Side(1, count), number).safeGet
          count + number - 2
      })
      t
    case _ => throw new Error
  }

  /**
    * all tessellations grown from start to given vertex adding one p-gon at a time
    *
    * @param v vertex
    * @return
    */
  def scanVertex(v: Vertex): List[Tiling] = v.edgesNumbers match {
    case edgesNumber :: numbers =>
      val t: Tiling = regPgonTiling(edgesNumber).safeGet
      val (ts, _) = numbers
        .foldLeft(List(t), edgesNumber)({
          case ((tilings, count), number) =>
            val c = tilings.head.clone()
            c.addToEdgePgon2(Side(1, count), number).safeGet
            (c +: tilings, count + number - 2)
        })
      ts.reverse
    case _ => throw new Error
  }

  /**
    * grow tessell of given size by respecting only 1 pattern
    *
    * @param pattern  full vertex
    * @param size     new size in p-gons
    * @param infinite if true, it should return Failure early if an unsuitable angle for growth is spotted
    * @return
    */
  def expandPattern(pattern: Full, size: Int = 100, infinite: Boolean = false): Try[Tiling] =
    pattern.pgonsNumber match {
      case less if size <= less => Try(fromVertex(Vertex(pattern.ps.take(size))))
      case n =>
        val t = fromVertex(pattern)
        t.expPatterns(List(pattern), size - n, infinite).map(_ => t)
    }

  /**
    * all tessellations from 1 to given size grown by respecting only 1 pattern
    *
    * @param pattern vertex
    * @param size    new size in p-gons
    * @return
    */
  def scanPattern(pattern: Full, size: Int = 100): List[Try[Tiling]] = pattern.pgonsNumber match {
    case less if size <= less => scanVertex(Vertex(pattern.ps.take(size))).map(Try(_))
    case n =>
      scanVertex(Vertex(pattern.ps.take(n - 1))).map(Try(_)) ++
        fromVertex(pattern).scanPatterns(List(pattern), size - n)
  }
}
