package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.Graph
import scalax.collection.GraphPredef._

import vision.id.tessella.Alias.Tiling

class growthTest extends FlatSpec with TilingUtils {

  "A vertex" can "be grown into a tessellation" in {
    val result = Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1, 7 ~ 8, 8 ~ 2)
    assert(Tiling.fromVertex(Vertex.s("(3*2.4.3.4)")).toG === result)
  }

  it can "be grown into a collection of tessellations adding one polygon at a time" in {
    val result = List(
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1, 7 ~ 8, 8 ~ 2)
    )
    assert(Tiling.scanVertex(Vertex.s("(3*2.4.3.4)")).map(_.toG) === result)
  }

}
