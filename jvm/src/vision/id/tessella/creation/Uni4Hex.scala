package vision.id.tessella.creation

import vision.id.tessella.Alias.Tiling

/**
  * fast methods to create reticulate tessellations of arbitrary size
  */
trait Uni4Hex extends Reticulate {

  /**
    * 4-uniform tessellation (▲⁶; ▲⁴.⬣; ▲².⬣²; ⬣³)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#4-uniform_tilings,_4_vertex_types
    */
  def fourUniformOneOneOneOne(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => (i + 2 * j) % 6 < 2)

  /**
    * 4-uniform tessellation (▲⁶; ▲⁴.⬣; ▲².⬣²; ⬣³)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#4-uniform_tilings,_4_vertex_types
    */
  def fourUniformOneOneOneOne1(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => (i + 2 * j) % 5 < 2)

  /**
    * 4-uniform tessellation (▲⁶; ▲⁴.⬣; ▲².⬣²; ⬣³)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#4-uniform_tilings,_4_vertex_types
    */
  def fourUniformOneOneOneOne2(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (_, j) => j % 3 == 0)

  /**
    * 4-uniform tessellation (▲⁶; ▲².⬣²; [2x ⬣³])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#4-uniform_tilings,_3_vertex_types_(2:1:1)
    */
  def fourUniformTwoOneOne(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => i % 7 == (j * 4) % 7)

  /**
    * 4-uniform tessellation (▲⁶; ▲².⬣²; [2x ⬣³])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#4-uniform_tilings,_3_vertex_types_(2:1:1)
    * @see http://probabilitysports.com/tilings.html?u=0&n=4&t=1
    */
  def fourUniformTwoOneOne2(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => j % 2 == 0 && i % 6 == (j * 4) % 6)

  /**
    * 4-uniform tessellation (▲⁶; ▲².⬣²; [2x ⬣³])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#4-uniform_tilings,_3_vertex_types_(2:1:1)
    */
  def fourUniformTwoOneOne3(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => ((i + 2 * j) % 6 == 0 || (i + 2 * j) % 6 == 2) && j % 2 == 0)

  /**
    * 4-uniform tessellation (▲⁶; [2x ▲².⬣²]; ⬣³)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#4-uniform_tilings,_3_vertex_types_(2:1:1)
    */
  def fourUniformTwoOneOne4(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => (i + 2 * j) % 3 == 0 && j % 3 < 2)

  /**
    * 4-uniform tessellation (▲⁶; [2x ▲².⬣²]; ⬣³)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#4-uniform_tilings,_3_vertex_types_(2:1:1)
    */
  def fourUniformTwoOneOne5(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => {
      // alternate rows between 3 and 6
      val step = (j % 2 + 1) * 3
      (i + 2 * j) % step == 0
    })

  /**
    * 4-uniform tessellation (▲⁶; ▲².⬣²; [2x ⬣³])
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#4-uniform_tilings,_3_vertex_types_(2:1:1)
    */
  def fourUniformTwoOneOne6(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, _ % 3 == 0 && _ % 3 == 0)

  /**
    * 4-uniform tessellation (▲⁶; [2x ▲⁴.⬣]; ▲².⬣²)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#4-uniform_tilings,_3_vertex_types_(2:1:1)
    */
  def fourUniformTwoOneOne7(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => {
      val pos = i + 2 * j
      if (j % 2 == 0)
        pos % 3 < 2
      else
        pos % 6 == 0 || pos % 6 == 4
    })

}
