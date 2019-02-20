package vision.id.tessella

import org.scalameter._
import org.scalatest.FlatSpec
import vision.id.tessella.Tessella.Tiling

class perimeterBench extends FlatSpec with TilingUtils with Loggable {

  setLogLevel("WARN")

  val t: Tiling = Tiling.threeUniformOneOneOne8(10, 10)

  "Method setPerimeter" must "execute in less than 25 milliseconds" in {
    val time = config(
      Key.exec.benchRuns -> 5,
      Key.verbose        -> true
      //    ) measure {
    ) withWarmer {
      new Warmer.Default
      //    } withMeasurer {
      //      new Measurer.IgnoringGC
    } measure {
      t.setPerimeter
    }
    assert(time.value < 25.0)

  }

  "Method toNodesMap" must "execute in less than 200 milliseconds" in {
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
    assert(time.value < 200.0)

  }

  val nm: NodesMap = t.toNodesMap

  "Method toPolygons" must "execute in less than 5 seconds" in {
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
    assert(time.value < 5000.0)

  }

}
