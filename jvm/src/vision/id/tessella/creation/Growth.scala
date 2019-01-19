package vision.id.tessella.creation

import scala.util.Try

import vision.id.tessella.{RegPgon, Side, TilingUtils, Vertex}
import vision.id.tessella.Alias.Tiling

/**
  * slow methods to create tessellations with "organic" growth
  */
trait Growth extends TilingUtils {

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

}
