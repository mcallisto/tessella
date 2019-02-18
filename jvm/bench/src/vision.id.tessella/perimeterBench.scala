package vision.id.tessella

import org.scalameter._
import org.scalatest.FlatSpec
import vision.id.tessella.Tessella.Tiling

class perimeterBench extends FlatSpec with TilingUtils with Loggable {

  setLogLevel("WARN")

  val t: Tiling = Tiling.threeUniformOneOneOne8(10, 10)

  "Method getPerimeter" must "execute in less than 40 milliseconds" in {
    val time = config(
      Key.exec.benchRuns -> 5,
      Key.verbose        -> true
      //    ) measure {
    ) withWarmer {
      new Warmer.Default
      //    } withMeasurer {
      //      new Measurer.IgnoringGC
    } measure {
      t.perimeterEdges
    }
    assert(time.value < 40.0)

  }

  "Method toNodesMap" must "execute in less than 250 milliseconds" in {
    val time = config(
      Key.exec.benchRuns -> 5,
      Key.verbose        -> true
      //    ) measure {
    ) withWarmer {
      new Warmer.Default
      //    } withMeasurer {
      //      new Measurer.IgnoringGC
    } measure {
      t.toNodesMap
    }
    assert(time.value < 250.0)

  }

  val nm: NodesMap = t.toNodesMap

  "Method toPolygons" must "execute in less than 11 seconds" in {
    val time = config(
      Key.exec.benchRuns -> 5,
      Key.verbose        -> true
      //    ) measure {
    ) withWarmer {
      new Warmer.Default
      //    } withMeasurer {
      //      new Measurer.IgnoringGC
    } measure {
      t.toPolygons(nm)
    }
    assert(time.value < 11000.0)

  }

}
