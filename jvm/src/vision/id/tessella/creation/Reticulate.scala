package vision.id.tessella.creation

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

import vision.id.tessella.{Alias, GraphUtils, Methods}
import Alias.Tiling

/**
  * fast methods to create reticulate tessellations of arbitrary size
  */
trait Reticulate extends GraphUtils with Methods {

  private def toEdges(outerNodes: IndexedSeq[(Int, Int)]): IndexedSeq[UnDiEdge[Int]] =
    outerNodes.map({ case (node1, node2) => node1 ~ node2 })

  /**
    * compose a rectangular reticulate of x by y squares
    *
    * @param x side units
    * @param y side units
    * @return
    */
  def squareNet(x: Int, y: Int): Tiling = {
    require(x > 0 && y > 0)

    val horiz = for {
      i <- 1 to x
      j <- 0 to y
      h = i + (x + 1) * j
    } yield (h, h + 1)
    val vert = for {
      i <- 1 to (x + 1)
      j <- 0 until y
    } yield (i + (x + 1) * j, i + (x + 1) * (j + 1))
    Tiling.fromG(Graph.from(edges = toEdges(horiz ++ vert)))
  }

  /**
    * compose a rectangular reticulate of x by y triangles
    *
    * @param x side units
    * @param y side units
    * @return
    */
  def triangleNet(x: Int, y: Int): Tiling = {
    require(x > 0 && y > 0)
    require(x % 2 == 0)

    val h = x / 2
    val diag = for {
      i <- 1 to h
      j <- 0 until y
    } yield (i + (h + 1) * j, i + 1 + (h + 1) * (j + 1))
    squareNet(h, y) ++ toEdges(diag)
  }

  /**
    * compose a rectangular reticulate of x by y hexagons
    *
    * @param x side units
    * @param y side units
    * @return
    */
  def hexagonNet(x: Int, y: Int): Tiling = {
    require(x > 0 && y > 0)

    val horiz = for {
      i <- 0 to x * 2
      j <- 0 to y
      h = i + (x + 1) * j * 2
    } yield (h, h + 1)
    val vert = for {
      i <- 1 to (x + 1)
      j <- 0 until y
      v = (x + 1) * 2; w = i * 2 - 1
    } yield (w + v * j, w - 1 + v * (j + 1))
    Tiling.fromG(Graph.from(edges = toEdges(horiz.tail.init ++ vert)))
  }

  /**
    * build variants of triangle grid by emptying hex according to function
    *
    * @param x number of triangles on the x-axis
    * @param y number of triangles on the y-axis
    * @param f function telling if given ij node must be deleted or not
    * @return
    */
  private def triangleNetVariant(x: Int, y: Int, f: (Int, Int) => Boolean): Tiling = {
    val h = x / 2
    val emptyNodes = for {
      i <- 1 to h + 1
      j <- 0 to y
      if f(i, j)
    } yield i + (h + 1) * j
    val newg    = emptyNodes.foldLeft(triangleNet(x, y))(_ - _)
    val orphans = newg.nodes filter (_.degree == 1)

    def cutCorners(g: Graph[Int, UnDiEdge], bridges: List[Int], corners: List[Int]): Graph[Int, UnDiEdge] = {
      if (g.nodes.toList.map(_.toOuter).intersect(bridges).isEmpty)
        g -- Graph.from(corners, Nil).nodes
      else
        g
    }

    val bottomleft = List(1 + (y - 2) * (h + 1), 3 + y * (h + 1))
    val blCorners = List(1 + (y - 1) * (h + 1), 2 + y * (h + 1), 1 + y * (h + 1))

    val topright   = List(3 * (h + 1), h - 1)
    val trCorners = List(2 * (h + 1), h, h + 1)

    Tiling.fromG(cutCorners(cutCorners(newg -- orphans, bottomleft, blCorners), topright, trCorners).renumber())
  }

  /**
    * uniform Tessellation (▲.⬣.▲.⬣) (t=2, e=1)
    */
  def uniform(x: Int, y: Int): Tiling =
    triangleNetVariant(x, y, _ % 2 == 1 && _ % 2 == 1)

  /**
    * uniform Tessellation (▲⁴.⬣) (t=3, e=3)
    */
  def uniform2(x: Int, y: Int): Tiling =
    triangleNetVariant(x, y, (i, j) => (i + 2 * j) % 7 == 0)

  /**
    * 2-uniform Tessellation (▲⁶; ▲⁴.⬣) (t=5, e=7)
    */
  def twoUniform3(x: Int, y: Int): Tiling =
    triangleNetVariant(x, y, (i, j) => (i + 3 * j) % 13 == 0)

  /**
    * 2-uniform Tessellation (▲⁴.⬣; ▲².⬣²) (t=2, e=4)
    */
  def twoUniform4(x: Int, y: Int): Tiling =
    triangleNetVariant(x, y, (i, j) => (i + 3 * j) % 5 == 0)

  /**
    * 2-uniform Tessellation (▲.⬣.▲.⬣; ▲².⬣²) (t=2, e=3)
    */
  def twoUniform5(x: Int, y: Int): Tiling =
    triangleNetVariant(x, y, (i, j) => (i + 2 * j) % 4 == 0)

  /**
    * 3-uniform Tessellation (▲².⬣²; ▲.⬣.▲.⬣; ⬣³) (t=4, e=5)
    */
  def threeUniformOneOneOne4(x: Int, y: Int): Tiling = {
    val f: (Int, Int) => Boolean = (i, j) =>
      j % 5 match {
        case e if e == 2 || e == 4 => i           % 5 == (e / 2 + 1) || i % 5 == (e / 2 - 1)
        case _                     => (i + 2 * j) % 5 == 0
    }
    triangleNetVariant(x, y, f)
  }

  /**
    * 3-uniform Tessellation (▲².⬣²; ▲.⬣.▲.⬣; ⬣³) (t=2, e=4)
    */
  def threeUniformOneOneOne5(x: Int, y: Int): Tiling =
    triangleNetVariant(x, y, (i, j) => (i + 3 * j) % 7 == 0 || (i + 3 * j) % 7 == 2)

  /**
    * 3-uniform Tessellation (▲⁶; ▲⁴.⬣; ▲.⬣.▲.⬣) (t=5, e=6)
    */
  def threeUniformOneOneOne6(x: Int, y: Int): Tiling =
    triangleNetVariant(x, y, (i, j) => (i + 4 * j) % 8 == 0)

  /**
    * 4-uniform Tessellation ([2x ▲⁶]; ▲⁴.⬣; ▲².⬣²)
    */
  def fourUniformTwoOneOne8(x: Int, y: Int): Tiling =
    triangleNetVariant(x, y, (i, j) => (i + 1 * j) % 8 == 0)

  /**
    * get nodes of a single hex on the hexagonNet
    *
    * @param x number of hexs on the x-axis
    * @param i cell coord on the x-axis (0 first)
    * @param j cell coord on the y-axis (0 first)
    * @return
    */
  private def hexagonNetCellNodes(x: Int, i: Int, j: Int): List[Int] =
    for {
      q <- List(0, 1)
      p <- List(0, 1, 2)
    } yield (j + q) * ((x + 1) * 2) + i * 2 + p + 1 - q

  /**
    * build variants of hex grid by filling hex with ▲⁶ according to function
    *
    * @param x number of hexs on the x-axis
    * @param y number of hexs on the y-axis
    * @param f function telling if given ij hex must be filled or not
    * @return
    */
  def hexagonNetVariant(x: Int, y: Int, f: (Int, Int) => Boolean): Tiling = {
    val totNodes = (y + 1) * 2 * (x + 1) - 2
    val hexNodes = for {
      i <- 0 until x
      j <- 0 until y
      if f(i, j)
    } yield hexagonNetCellNodes(x, i, j)
    val (newg, _) = hexNodes.foldLeft((hexagonNet(x, y).toG, totNodes + 1))({
      case ((g, centre), hex) =>
        (hex.foldLeft(g)((h, hexNode) => h + centre ~ hexNode), centre + 1)
    })
    Tiling.fromG(newg)
  }

  /**
    * 2-uniform tessellation (▲⁶; ▲².⬣²) (t=2, e=3)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#2-uniform_tilings
    */
  def twoUniform(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, _ % 3 == _ % 3)

  /**
    * 2-uniform tessellation (▲⁶; ▲⁴.⬣) (t=3, e=3)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#2-uniform_tilings
    */
  def twoUniform2(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, _ % 3 != _ % 3)
  //triangleNetVariant(x, y, _ % 3 == 0 && _ % 3 == 0)

  /**
    * 3-uniform tessellation (▲⁶; ⬣³; ▲².⬣²) (t=2, e=3)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#3-uniform_tilings,_3_vertex_types
    */
  def threeUniformOneOneOne(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, _ % 2 == 0 && _ % 2 == 0)

  /**
    * 3-uniform tessellation (▲⁶; ▲⁴.⬣; ▲².⬣²) (t=5, e=8)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#3-uniform_tilings,_3_vertex_types
    */
  def threeUniformOneOneOne2(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => (i + 2 * j) % 4 < 2)

  /**
    * 3-uniform tessellation (▲⁶; ▲⁴.⬣; ▲².⬣²) (t=3, e=5)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#3-uniform_tilings,_3_vertex_types
    */
  def threeUniformOneOneOne3(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (_, j) => j % 2 == 0)

  /**
    * 3-uniform tessellation ([2x ▲⁶]; ▲⁴.⬣) (t=3, e=4)
    *
    * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#3-uniform_tilings,_2_vertex_types_(2:1)
    */
  def threeUniformTwoOne(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, _ % 2 == 0 || _ % 2 == 0)

  /**
    * 6-uniform tessellation (▲⁶; ▲².⬣²; [4x ⬣³])
    *
    * @see http://probabilitysports.com/tilings.html?u=0&n=6&t=2
    */
  def sixUniformFourOneOne(x: Int, y: Int): Tiling =
    hexagonNetVariant(x, y, (i, j) => i % 10 == (j * 8) % 10)

}
