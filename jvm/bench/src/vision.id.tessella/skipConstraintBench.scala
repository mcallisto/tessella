package vision.id.tessella

import org.scalameter._
import org.scalatest.FlatSpec
import scalax.collection.GraphEdge.UnDiEdge
import vision.id.tessella.Tessella.Tiling

class skipConstraintBench extends FlatSpec with AddUtils {

  val t1: Tiling = Tiling.fromG(Tiling.hexagonNet(10, 10).toG - 4)

  "Adding a pgon (elements forming a single path) substituting just one perimeter edge" must "must execute in less than 100 ms" in {
    val time = config(
      Key.exec.benchRuns -> 5,
      Key.verbose        -> true
    ) measure {
      t1 ++ Set(Side(1, 300), Side(300, 2))
    }
    assert(time.value < 100.0)

  }

  "Adding a pgon (elements forming a single path) substituting more than one perimeter edge" must "must execute in less than 300 ms" in {
    val time = config(
      Key.exec.benchRuns -> 5,
      Key.verbose        -> true
    ) measure {
      t1 ++ Set(Side(3, 4), Side(4, 5))
    }
    assert(time.value < 400.0)

  }

  "Adding edges not forming a single path" must "must execute in less than 400 ms" in {
    val time = config(
      Key.exec.benchRuns -> 5,
      Key.verbose        -> true
    ) measure {
      t1 ++ Set(Side(1, 300), Side(300, 2), Side(5, 301), Side(301, 6))
    }
    assert(time.value < 300.0)

  }

  val t2: Tiling = Tiling.fromG(Tiling.squareNet(10, 10).toG - UnDiEdge(2, 3))

  "Adding an edge linking two perimeter nodes" must "must execute in less than 300 ms" in {
    val time = config(
      Key.exec.benchRuns -> 5,
      Key.verbose        -> true
    ) measure {
      t2 + Side(2, 3)
    }
    assert(time.value < 300.0)

  }

}
