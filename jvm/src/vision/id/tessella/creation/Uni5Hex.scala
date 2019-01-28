package vision.id.tessella.creation

import vision.id.tessella.Tessella.Tiling

/**
  * fast methods to create reticulate tessellations of arbitrary size
  */
trait Uni5Hex extends Reticulate {

  /**
    * 5-uniform tessellation ([2x ▲⁶]; ▲⁴.⬣; ▲².⬣²; ⬣³)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_4_vertex_types_(2:1:1:1)
    */
  def fiveUniformTwoOneOneOne(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => {
      val pos = i + 2 * j
      if (j % 2 == 0)
        pos % 3 == 1
      else
        pos % 6 < 3
    })

  /**
    * 5-uniform tessellation (▲⁶; ▲⁴.⬣; ▲².⬣²; [2x ⬣³])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_4_vertex_types_(2:1:1:1)
    */
  def fiveUniformTwoOneOneOne2(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => (i + 2 * j) % 8 < 2)

  /**
    * 5-uniform tessellation (▲⁶; ▲⁴.⬣; [2x ▲².⬣²]; ⬣³)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_4_vertex_types_(2:1:1:1)
    */
  def fiveUniformTwoOneOneOne3(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => {
      val pos = i + 2 * j
      pos % 7 == 0 || pos % 7 == 2
    })

  /**
    * 5-uniform tessellation (▲⁶; ▲⁴.⬣; ▲².⬣²; [2x ⬣³])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_4_vertex_types_(2:1:1:1)
    */
  def fiveUniformTwoOneOneOne4(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => (i + 2 * j) % 7 < 2)

  /**
    * 5-uniform tessellation (▲⁶; ▲⁴.⬣; ▲².⬣²; [2x ⬣³])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_4_vertex_types_(2:1:1:1)
    */
  def fiveUniformTwoOneOneOne5(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => (i + j) % 4 == 0)

  /**
    * 5-uniform tessellation (▲⁶; ▲⁴.⬣; [2x ▲².⬣²]; ⬣³)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_4_vertex_types_(2:1:1:1)
    */
  def fiveUniformTwoOneOneOne6(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => {
      val pos = i + 2 * j
      pos % 6 == 0 || pos % 6 == 2
    })

  /**
    * 5-uniform tessellation (▲⁶; [3x ▲².⬣²]; ⬣³)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_3_vertex_types_(3:1:1)_and_(2:2:1)
    */
  def fiveUniformThreeOneOne(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => {
      val pos = i + 2 * j
      pos % 8 == 0 || pos % 8 == 3
    })

  /**
    * 5-uniform tessellation (▲⁶; [3x ▲².⬣²]; ⬣³)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_3_vertex_types_(3:1:1)_and_(2:2:1)
    */
  def fiveUniformThreeOneOne2(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => {
      val pos = i + 2 * j
      pos % 7 == 0 || pos % 7 == 3
    })

  /**
    * 5-uniform tessellation ([3x ▲⁶]; ▲⁴.⬣; ▲².⬣²)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_3_vertex_types_(3:1:1)_and_(2:2:1)
    */
  def fiveUniformThreeOneOne3(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => (i + 2 * j) % 6 > 1)

  /**
    * 5-uniform tessellation ([3x ▲⁶]; ▲⁴.⬣; ▲².⬣²)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_3_vertex_types_(3:1:1)_and_(2:2:1)
    */
  def fiveUniformThreeOneOne4(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => {
      val pos = i + 2 * j
      if (j % 2 == 0)
        pos % 3 < 2
      else
        pos % 6 < 1 || pos % 6 > 3
    })

  /**
    * 5-uniform tessellation (▲⁶; [2x ▲².⬣²]; [2x ⬣³])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_3_vertex_types_(3:1:1)_and_(2:2:1)
    * @see http://probabilitysports.com/tilings.html?u=0&n=5&t=2
    */
  def fiveUniformTwoTwoOne(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => i % 7 == (j * 3) % 7)

  /**
    * 5-uniform tessellation ([2x ▲⁶]; ▲⁴.⬣; [2x ▲².⬣²])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_3_vertex_types_(3:1:1)_and_(2:2:1)
    */
  def fiveUniformTwoTwoOne2(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => i % 3 == j % 3 || (i % 3 == 0 && j % 3 == 1))

  /**
    * 5-uniform tessellation (▲⁶; [2x ▲².⬣²]; [2x ⬣³])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_3_vertex_types_(3:1:1)_and_(2:2:1)
    */
  def fiveUniformTwoTwoOne3(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => (i + 2 * j) % 6 == 0)

  /**
    * 5-uniform tessellation ([2x ▲⁶]; [2x ▲⁴.⬣]; ▲².⬣²)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_3_vertex_types_(3:1:1)_and_(2:2:1)
    */
  def fiveUniformTwoTwoOne4(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => {
      val pos = i + 2 * j
      pos % 5 == 1 || pos % 5 > 2
    })

  /**
    * 5-uniform tessellation ([4x ▲⁶]; ▲⁴.⬣)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_2_vertex_types_(4:1)_and_(3:2)
    */
  def fiveUniformFourOne(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, _ % 3 != 0 || _ % 3 != 0)

}
