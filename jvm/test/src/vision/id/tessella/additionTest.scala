package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.GraphPredef._

import vision.id.tessella.Alias.Tiling

class additionTest extends FlatSpec with AddUtils with OptionUtils with TryUtils {

  val uShape: Tiling = Tiling.fromG(Tiling.squareNet(3, 2).toG - 2 ~ 3)

  "A tessellation" can "have a polygon added replacing three perimeter edges" in {
    val attachEdge = uShape.edges.find(_.toOuter.toList.sorted == List(6, 7)).safeGet().toOuter
    assert(uShape.addToEdgePgon(attachEdge, 4).safeGet.edges.toString ===
      "EdgeSet(9-10, 1-2, 1-5, 5-9, 5=6, 2-3, 2=6, 6=7, 6=10, 3-4, 3=7, 10-11, 7=11, 7=8, 4-8, 11-12, 8-12)")
  }

  it can "have another polygon added replacing one perimeter edge" in {
    val attachEdge = uShape.edges.find(_.toOuter.toList.sorted == List(1, 5)).safeGet().toOuter
    assert(uShape.addToEdgePgon(attachEdge, 4).safeGet.edges.toString ===
      "EdgeSet(9-10, 1-2, 1=5, 2-6, 3-4, 3-7, 10-11, 4-8, 11-12, 13-14, 5-9, 5=6, 5-13, 6-7, 6=10, 7=11, 7=8, 14-1, 8-12)")
  }

}
