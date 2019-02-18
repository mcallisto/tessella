package vision.id.tessella

import org.scalameter._
import org.scalatest.FlatSpec

import vision.id.tessella.Tessella.Tiling

class pgonsMapBench extends FlatSpec with TilingUtils with Loggable {

  setLogLevel("WARN")

  "Method pgonsMap" must "execute in less than 1.5 seconds" in {
    val time = config(
      Key.exec.benchRuns -> 10,
      Key.verbose        -> true
//    ) measure {
    ) withWarmer {
      new Warmer.Default
//    } withMeasurer {
//      new Measurer.IgnoringGC
    } measure {
      Tiling.threeUniformOneOneOne8(6, 6).pgonsMap
    }
    assert(time.value < 1500.0)

  }

}
