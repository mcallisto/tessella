package vision.id.tessella

import org.scalatest.FlatSpec
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import vision.id.tessella.Others.Mono
import vision.id.tessella.Tessella.Tiling

class growthTest extends FlatSpec with TilingUtils with TryUtils with Loggable {

  setLogLevel("WARN")

  "A vertex" can "be grown into a tessellation" in {
    val result = Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1, 7 ~ 8, 8 ~ 2)
    assert(Full.s("(3*2.4.3.4)").toTiling.toG === result)
  }

  it can "be grown into a collection of tessellations adding one polygon at a time" in {
    val result = List(
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 3 ~ 4, 4 ~ 1, 4 ~ 5, 5 ~ 6, 6 ~ 1, 6 ~ 7, 7 ~ 1, 7 ~ 8, 8 ~ 2)
    )
    assert(Full.s("(3*2.4.3.4)").toScan.map(_.toG) === result)
  }

  "An empty vertex" must "be grown into an empty tessellation" in {
    assert(new Vertex(Nil).toTiling.isEmpty)
  }

  it must "be grown into an empty collection of tessellations" in {
    assert(new Vertex(Nil).toScan.isEmpty)
  }

  "A full vertex" can "be expanded into a 1-uniform tessellation" in {
    assert(
      Mono.expandPattern(Full.s("(3*2.4.3.4)"), 20).safeGet.toG === Graph(
        1 ~ 2,
        2 ~ 3,
        2 ~ 9,
        2 ~ 11,
        3 ~ 1,
        3 ~ 4,
        3 ~ 12,
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
        8 ~ 22,
        9 ~ 10,
        10 ~ 3,
        11 ~ 8,
        11 ~ 9,
        12 ~ 10,
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
        20 ~ 8,
        20 ~ 21,
        21 ~ 18,
        22 ~ 23,
        23 ~ 11
      ))
    val t = Mono.expandPattern(Full.s("(3.4.6.4)"), 10).safeGet
    assert(
      t.edges.toString === "EdgeSet(15-10, 9-10, 9=1, 1=2, 16-17, 2=3, 2=11, 17-18, 3=1, 3=4, 18=4, 10=2, 4=5, 4=19, 11-12, 11-13, 12=3, 12-16, 19=5, 19-20, 19-21, 13-14, 5=6, 5=1, 20-18, 6-7, 21-6, 7-8, 14-15, 8-9)")
  }

  it can "be grown into a tessellation" in {
    val t = Full.s("(3.3.4.3.4)").toTiling
    assert(t.edges.toString === "EdgeSet(1=2, 5-6, 2-3, 6=1, 6-7, 3=1, 3-4, 7=1, 7-8, 4=1, 4-5, 8-2)")
  }

  it can "be scanned into many tessellations growing 1 pgon at a time" in {
    val ts = Full.s("(3.3.4.3.4)").toScan
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
    val ts = Mono.scanPattern(Full.s("(3.3.4.3.4)"), size = 10)
    assert(
      ts.map(_.safeGet.edges.toString) === List(
        "EdgeSet(1-2, 2-3, 3-1)",
        "EdgeSet(1-2, 2-3, 3=1, 3-4, 4-1)",
        "EdgeSet(1-2, 5-6, 2-3, 6-1, 3=1, 3-4, 4=1, 4-5)",
        "EdgeSet(1-2, 5-6, 2-3, 6-7, 6=1, 3=1, 3-4, 7-1, 4=1, 4-5)",
        "EdgeSet(1=2, 5-6, 2-3, 6-7, 6=1, 3=1, 3-4, 7=1, 7-8, 4=1, 4-5, 8-2)",
        "EdgeSet(9-10, 1=2, 5-6, 2=3, 2-9, 6-7, 6=1, 3=1, 3-4, 10-3, 7=1, 7-8, 4=1, 4-5, 8-2)",
        "EdgeSet(9-10, 1=2, 5-6, 2=3, 2-11, 2-9, 6-7, 6=1, 3=1, 3-4, 10-3, 7=1, 7-8, 4=1, 4-5, 11-8, 8=2)",
        "EdgeSet(9-10, 1=2, 5-6, 2=3, 2=11, 2=9, 6-7, 6=1, 3=1, 3-4, 10-3, 7=1, 7-8, 4=1, 4-5, 11-9, 11-8, 8=2)",
        "EdgeSet(12-13, 9-10, 1=2, 13-4, 5-6, 2=3, 2=11, 2=9, 6-7, 6=1, 3=1, 3=4, 3-12, 10-3, 7=1, 7-8, 4=1, 4-5, 11-9, 11-8, 8=2)",
        "EdgeSet(12-13, 12-10, 9-10, 1=2, 13-4, 5-6, 2=3, 2=11, 2=9, 6-7, 6=1, 3=1, 3=4, 3=12, 10=3, 7=1, 7-8, 4=1, 4-5, 11-9, 11-8, 8=2)"
      ))
  }

}
