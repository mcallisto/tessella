package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.Graph
import scalax.collection.GraphPredef._

import vision.id.tessella.Tessella.Tiling

class growthTest extends FlatSpec with TilingUtils with TryUtils with Loggable {

  setLogLevel("WARN")

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

  "A full vertex" can "be expanded into a 1-uniform tessellation" in {
    assert(
      Tiling.expandPattern(Full.s("(3*2.4.3.4)"), 20).safeGet.toG === Graph(
        1 ~ 2,
        2 ~ 3,
        2 ~ 9,
        2 ~ 11,
        3 ~ 1,
        3 ~ 4,
        4 ~ 1,
        4 ~ 5,
        4 ~ 14,
        5 ~ 6,
        5 ~ 15,
        5 ~ 16,
        6 ~ 1,
        6 ~ 7,
        6 ~ 18,
        7 ~ 1,
        7 ~ 8,
        7 ~ 20,
        8 ~ 2,
        9 ~ 10,
        10 ~ 3,
        10 ~ 12,
        11 ~ 8,
        11 ~ 9,
        12 ~ 3,
        12 ~ 13,
        13 ~ 4,
        14 ~ 5,
        14 ~ 13,
        15 ~ 6,
        16 ~ 15,
        16 ~ 17,
        17 ~ 14,
        18 ~ 7,
        18 ~ 19,
        19 ~ 15,
        19 ~ 22,
        20 ~ 8,
        20 ~ 21,
        21 ~ 18,
        22 ~ 18
      ))
    val t = Tiling.expandPattern(Full.s("(3.4.6.4)"), 10).safeGet
    assert(
      t.edges.toString === "EdgeSet(15=3, 15-16, 9=1, 9-10, 1=2, 16-17, 2=3, 17-18, 3=1, 3=4, 18=4, 18-19, 10=2, 10-11, 4=5, 11-12, 12-13, 19-20, 13-14, 5=1, 5=6, 20=4, 20=5, 20-21, 6-7, 21-6, 7-8, 14=2, 14-15, 8-9)")
  }

  it can "be grown into a tessellation" in {
    val t = Tiling.fromVertex(Full.s("(3.3.4.3.4)"))
    assert(t.edges.toString === "EdgeSet(1=2, 5-6, 2-3, 6=1, 6-7, 3=1, 3-4, 7=1, 7-8, 4=1, 4-5, 8-2)")
  }

  it can "be scanned into many tessellations growing 1 pgon at a time" in {
    val ts = Tiling.scanVertex(Full.s("(3.3.4.3.4)"))
    assert(
      ts.map(_.edges.toString) === List(
        "EdgeSet(1-2, 2-3, 3-1)",
        "EdgeSet(1-2, 2-3, 3=1, 3-4, 4-1)",
        "EdgeSet(1-2, 5-6, 2-3, 6-1, 3=1, 3-4, 4=1, 4-5)",
        "EdgeSet(1-2, 5-6, 2-3, 6=1, 6-7, 3=1, 3-4, 7-1, 4=1, 4-5)",
        "EdgeSet(1=2, 5-6, 2-3, 6-7, 6=1, 3=1, 3-4, 7=1, 7-8, 4=1, 4-5, 8-2)"
      ))
  }

  it can "be scanned into many 1-uniform tessellations growing 1 pgon at a time" in {
    val ts = Tiling.scanPattern(Full.s("(3.3.4.3.4)"), size = 10)
    assert(
      ts.map(_.safeGet.edges.toString) === List(
        "EdgeSet(1-2, 2-3, 3-1)",
        "EdgeSet(1-2, 2-3, 3=1, 3-4, 4-1)",
        "EdgeSet(1-2, 5-6, 2-3, 6-1, 3=1, 3-4, 4=1, 4-5)",
        "EdgeSet(1-2, 5-6, 2-3, 6=1, 6-7, 3=1, 3-4, 7-1, 4=1, 4-5)",
        "EdgeSet(1=2, 5-6, 2-3, 6=1, 6-7, 3=1, 3-4, 7=1, 7-8, 4=1, 4-5, 8-2)",
        "EdgeSet(9-10, 1=2, 5-6, 2=3, 2-9, 6-7, 6=1, 3=1, 3-4, 10-3, 7=1, 7-8, 4=1, 4-5, 8-2)",
        "EdgeSet(9-10, 1=2, 5-6, 2=3, 2-9, 2-11, 6-7, 6=1, 3=1, 3-4, 10-3, 7=1, 7-8, 4=1, 4-5, 11-8, 8=2)",
        "EdgeSet(9-10, 1=2, 5-6, 2=3, 2=11, 2=9, 6-7, 6=1, 3=1, 3-4, 10-3, 7=1, 7-8, 4=1, 4-5, 11-8, 11-9, 8=2)",
        "EdgeSet(12-3, 9-10, 1=2, 5-6, 2=3, 2=11, 2=9, 6-7, 6=1, 3=1, 3-4, 10=3, 10-12, 7=1, 7-8, 4=1, 4-5, 11-9, 11-8, 8=2)",
        "EdgeSet(12=3, 12-13, 9-10, 1=2, 13-4, 5-6, 2=3, 2=11, 2=9, 6-7, 6=1, 3=1, 3=4, 10-12, 10=3, 7=1, 7-8, 4=1, 4-5, 11-9, 11-8, 8=2)"
      ))
  }

}
