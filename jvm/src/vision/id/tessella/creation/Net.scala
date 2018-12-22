package vision.id.tessella.creation

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge

import vision.id.tessella.Alias.Tiling

/**
  * add methods to create tessellations as repeated layers of rows
  */
trait Net extends Reticulate {

  /**
    * grid of 2 * l triangles with in between a emptied hexagon net
    *
    * @param l lenght of parallel sides
    * @param hex height of the hexagon net
    * @param f to empty hexagons in the net
    * @return
    */
  private def triHexBase(l: Int, hex: Int = 0, f: (Int, Int) => Boolean): (Tiling, Int) = {
    val topRight = l + 2
    val emptyNodes = for {
      i <- 1 to topRight
      j <- 1 to hex
      if f(i, j)
    } yield i + topRight * j
    val bottomLeft = topRight * (hex + 1) + 1
    val g          = triangleNet((l + 1) * 2, hex + 1) -- Graph.from(emptyNodes, Nil)
    val renumbered = (g -- List(topRight, bottomLeft)).renumber()
    (Tiling.fromG(renumbered), renumbered.nodes.map(_.toOuter).max - l)
  }

  /**
    * grid of 2 * l triangles with in between a full hexagon net
    *
    * @param l lenght of parallel sides
    * @param hex height of the hexagon net
    * @param step position of starting hexagon
    * @return
    */
  private def hexAreaRow(l: Int, hex: Int = 0, step: Int = 0): (Tiling, Int) = {
    val invertedStep = 3 - step % 3
    triHexBase(l, hex, (i, j) => (i + invertedStep + j % 3) % 3 == 0)
  }

  private val fs: Map[Int, Int => (Tiling, Int)] = Map(
    3 -> { l =>
      (triangleNet(l * 2, 1), l + 2)
    },
    4 -> { l =>
      (squareNet(l, 1), l + 2)
    },
    36   -> { triHexBase(_, 1, (i, _) => i % 2 == 1) },
    63   -> { triHexBase(_, 1, (i, _) => i % 2 == 0) },
    336  -> { triHexBase(_, 1, (i, _) => i % 3 == 1) },
    363  -> { triHexBase(_, 1, (i, _) => i % 3 == 0) },
    633  -> { triHexBase(_, 1, (i, _) => i % 3 == 2) },
    3362 -> { hexAreaRow(_, 2, 2) },
    3632 -> { hexAreaRow(_, 2, 1) },
    6332 -> { hexAreaRow(_, 2, 0) }
  )

  private def netVariant(layers: List[Int], l: Int, r: Int): Tiling = {

    val size = layers.size

    require(size > 0)
    require(fs.keys.toList.intersect(layers.distinct).sorted == layers.distinct.sorted)

    val (g, _) = (0 until r).foldLeft((Graph.empty[Int, UnDiEdge], 1))({
      case ((gg, start), row) =>
        val (t, s) = fs(layers(row % size))(l)
        (gg ++ t.renumber(start), start + s - 1)
    })
    Tiling.fromG(g)
  }

  /**
    * uniform tessellation (▲³.■²) (t=2, e=3)
    *
    * @see https://en.wikipedia.org/wiki/Elongated_triangular_tiling
    */
  def elongatedTriangular(l: Int, r: Int): Tiling =
    netVariant(List(3, 4), l, r)

  /**
    * uniform tessellation (▲.⬣.▲.⬣) (t=2, e=1)
    *
    * @see https://en.wikipedia.org/wiki/Trihexagonal_tiling
    */
  def triHexagonal(l: Int, r: Int): Tiling =
    netVariant(List(36, 63), l, r)

  /**
    * uniform tessellation (▲⁴.⬣; ▲³.■²; ▲.■².⬣)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_2_vertex_types_(4:1)_and_(3:2)
    */
  def threeUniformOneOneOne7(l: Int, r: Int): Tiling =
    netVariant(List(4, 363), l, r)

  /**
    * uniform tessellation (▲³.■²; ▲².⬣²; ▲.■².⬣) (t=5, e=8)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#3-uniform_tilings,_3_vertex_types
    */
  def threeUniformOneOneOne8(l: Int, r: Int): Tiling =
    netVariant(List(4, 3632, 4, 6332, 4, 3362), l, r)

  /**
    * uniform tessellation (▲³.■²; [2x ▲².⬣²]; [2x ▲.■².⬣])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_3_vertex_types_(3:1:1)_and_(2:2:1)
    */
  def fiveUniformTwoTwoOne5(l: Int, r: Int): Tiling =
    netVariant(List(4, 6332), l, r)

  /**
    * uniform tessellation ([4x ▲.⬣.▲.⬣]; ▲.■².⬣)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_2_vertex_types_(4:1)_and_(3:2)
    */
  def fiveUniformFourOne2(l: Int, r: Int): Tiling =
    netVariant(List(4, 36, 63, 36, 63), l, r)

  /**
    * uniform tessellation ([4x ▲.⬣.▲.⬣]; ▲.■².⬣)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_2_vertex_types_(4:1)_and_(3:2)
    */
  def fiveUniformFourOne3(l: Int, r: Int): Tiling =
    netVariant(List(4, 36, 63, 36, 63, 4, 63, 36, 63, 36), l, r)
}
