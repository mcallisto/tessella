package vision.id.tessella.creation

import scala.util.Try
import vision.id.tessella._
import vision.id.tessella.Tessella.Tiling

/**
  * slow methods to create tessellations with "organic" growth
  */
trait Growth extends AddUtils {

  private def regPgonTiling(edgesNumber: Int): Try[Tiling] = RegPgon.ofEdges(edgesNumber).map(_.toTiling)

  private def fAddVertexToEdge: ((Try[Tiling], Int), Int) => (Try[Tiling], Int) = {
    case ((tiling, count), edgesNumber) =>
      (tiling.flatMap(_.addToEdgePgon(Side(1, count), edgesNumber)), count + edgesNumber - 2)
  }

  /**
    * grow tessellation from vertex
    *
    * @param v vertex
    * @return
    */
  def fromVertex(v: Vertex): Tiling = v.edgesNumbers match {
    case edgesNumber :: numbers =>
      numbers.foldLeft((regPgonTiling(edgesNumber), edgesNumber))(fAddVertexToEdge) match {
        case (tiling, _) => tiling.safeGet
      }
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
      numbers
        .scanLeft((regPgonTiling(edgesNumber), edgesNumber))(fAddVertexToEdge)
        .map({
          case (tiling, _) => tiling.safeGet
        })
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
  def expandPattern(pattern: Full, size: Int = 100, infinite: Boolean = false): Try[Tiling] = pattern.pgonsNumber match {
    case less if size <= less => Try(fromVertex(Vertex(pattern.ps.take(size))))
    case n => fromVertex(pattern).expPatterns(List(pattern), size - n, infinite)
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
    case n => scanVertex(Vertex(pattern.ps.take(n - 1))).map(Try(_)) ++
      fromVertex(pattern).scanPatterns(List(pattern), size - n)
  }
}
