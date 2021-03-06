package vision.id.tessella

import scalax.collection.GraphEdge._
import scalax.collection.constrained.mutable._
import scalax.collection.constrained.Config

import vision.id.tessella.Others.Mono
import vision.id.tessella.creation.{Net, Reticulate, Uni4Hex, Uni5Hex}

object Tessella {

  type Tiling = Graph[Int, Side]

  implicit val conf: Config = ByRegularPgons

  object Tiling
      extends CompanionAlias[Side](ByRegularPgons withStringPrefix "Tiling")
      with Net
      with Reticulate
      with Uni4Hex
      with Uni5Hex {

    def fromSides(sides: Set[Side[Int]]): Tiling = from(Nil, sides).asInstanceOf[Tiling]

    def fromG(graph: scalax.collection.Graph[Int, UnDiEdge]): Tiling =
      from(graph.nodes.map(_.toOuter), graph.edges.map(edge => Side.fromEdge(edge.toOuter))).asInstanceOf[Tiling]

    def fromMono(mono: Mono): Tiling = fromSides(mono.edges.toOuter)

  }

}
