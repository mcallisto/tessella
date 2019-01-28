package vision.id.tessella

import scalax.collection.GraphEdge._
import scalax.collection.constrained.mutable._
import scalax.collection.constrained.Config

object Tessella {

  type TilingM = Graph[Int, Side]

  implicit val conf: Config = Shaped

  object TilingM extends CompanionAlias[Side](Shaped withStringPrefix "TilingM") {

    def fromSides(sides: Set[Side[Int]]): TilingM = from(Nil, sides).asInstanceOf[TilingM]

    def fromG(graph: scalax.collection.Graph[Int, UnDiEdge]): TilingM =
      TilingM.from(graph.nodes, graph.edges.map(edge => Side.fromEdge(edge.toOuter))).asInstanceOf[TilingM]

  }

}
