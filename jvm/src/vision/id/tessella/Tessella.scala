package vision.id.tessella

import scalax.collection.GraphEdge._
import scalax.collection.constrained.mutable._
import scalax.collection.constrained.Config

import vision.id.tessella.creation.{Growth, Net, Reticulate, Uni4Hex, Uni5Hex}

object Tessella {

  type Tiling = Graph[Int, Side]

  implicit val conf: Config = Shaped

  object Tiling
      extends CompanionAlias[Side](Shaped withStringPrefix "Tiling")
      with Growth
      with Net
      with Reticulate
      with Uni4Hex
      with Uni5Hex {

    def fromSides(sides: Set[Side[Int]]): Tiling = from(Nil, sides).asInstanceOf[Tiling]

    def fromG(graph: scalax.collection.Graph[Int, UnDiEdge]): Tiling =
      from(graph.nodes.map(_.toOuter), graph.edges.map(edge => Side.fromEdge(edge.toOuter))).asInstanceOf[Tiling]
  }

}
