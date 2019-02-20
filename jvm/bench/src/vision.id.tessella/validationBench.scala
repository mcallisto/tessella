package vision.id.tessella

import org.scalameter._
import org.scalatest.FlatSpec
import vision.id.tessella.Tessella.Tiling

class validationBench extends FlatSpec with TilingUtils with Loggable {

  setLogLevel("WARN")

  "Creating a Tiling" must "must execute in less than 7 seconds" in {
    val time = config(
      Key.exec.benchRuns -> 5,
      Key.verbose        -> true
//    ) measure {
    ) withWarmer {
      new Warmer.Default
//    } withMeasurer {
//      new Measurer.IgnoringGC
    } measure {
      Tiling.squareNet(30, 30)
    }
    assert(time.value < 7000.0)

  }

}
