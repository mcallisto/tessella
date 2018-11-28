package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.Graph
import scalax.collection.GraphPredef._ // shortcuts

class graphUtilsTest extends FlatSpec with GraphUtils {

  "A graph" can "have its node reordered starting from one" in {

    val g = Graph(10 ~ 20, 20 ~ 30, 30 ~ 10, 10 ~ 40, 40 ~ 50, 50 ~ 20, 60)

    assert(g.renumber() === Graph(1 ~ 2, 2 ~ 3, 3 ~ 1, 1 ~ 4, 4 ~ 5, 5 ~ 2, 6))
    assert(g.renumber(11) === Graph(11 ~ 12, 12 ~ 13, 13 ~ 11, 11 ~ 14, 14 ~ 15, 15 ~ 12, 16))
  }

}
