package vision.id.tessella.creation

import scala.util.Try

import com.typesafe.scalalogging.Logger

import scalax.collection.GraphPredef._
import vision.id.tessella.{ListUtils, Methods, OptionUtils, Vertex}
import vision.id.tessella.Alias.Tiling

/**
  * slow methods to create tessellations with "organic" growth
  */
trait Growth extends Methods with OptionUtils with ListUtils {

  val tlogger = Logger("GROWTH")

  /**
    * function adding polygon to vertex growth
    *
    * @return
    */
  private def fAddVertexToEdge: ((Try[Tiling], Int), Int) => (Try[Tiling], Int) = {
    case ((tess, count), sides) =>
      val t: Tiling = tess.safeGet
      (t.addToEdgePgon(1 ~ count, sides).flatMap(t => Try(t)), count + sides - 2)
  }

  /**
    * grow tessellation from vertex
    *
    * @param v vertex
    * @return
    */
  def fromVertex(v: Vertex): Tiling = {
    val h = v.psSides.safeHead
    v.psSides.tail.foldLeft((Try(Tiling.poly(h)), h))(fAddVertexToEdge) match {
      case (tess, _) => tess.safeGet
    }
  }

  /**
    * all tessellations from start to given vertex adding one poly at a time
    *
    * @param v vertex
    * @return
    */
  def scanVertex(v: Vertex): List[Tiling] = {
    val h = v.psSides.safeHead
    v.psSides.tail
      .scanLeft((Try(Tiling.poly(h)), h))(fAddVertexToEdge)
      .map({
        case (tess, _) => tess.safeGet
      })
  }

}
