package vision.id.tessella

import scalax.collection.GraphEdge._
import scalax.collection.constrained._
import scalax.collection.GraphPredef._

import vision.id.tessella.creation.{Growth, Net, Reticulate, Uni4Hex, Uni5Hex}

object Alias {

  type Tiling = Graph[Int, Side]

  implicit val conf: Config = Shaped

  object Tiling
      extends CompanionAlias[Side](Shaped withStringPrefix "Tiling")
      with Growth
      with Net
      with Reticulate
      with Uni4Hex
      with Uni5Hex {

    def fromSides(sides: Set[Side[Int]]): Tiling = Tiling.from(Nil, sides)

    def fromG(graph: scalax.collection.Graph[Int, UnDiEdge]): Tiling =
      Tiling.from(graph.nodes, graph.edges.map(edge => Side.fromEdge(edge.toOuter)))

  }

}
