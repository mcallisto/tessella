package vision.id.tessella

import scalax.collection.GraphEdge._
import scalax.collection.constrained.Config
import scalax.collection.constrained.mutable._

import vision.id.tessella.Tessella.Tiling
import vision.id.tessella.creation._

object Others {

  type Mono = Graph[Int, Side]

  implicit val conf: Config = Monogonal

  object Mono extends CompanionAlias[Side](Monogonal withStringPrefix "Mono") with Quadratic {

    def fromSides(sides: Set[Side[Int]]): Mono = from(Nil, sides).asInstanceOf[Mono]

    def fromG(graph: scalax.collection.Graph[Int, UnDiEdge]): Mono =
      from(graph.nodes.map(_.toOuter), graph.edges.map(edge => Side.fromEdge(edge.toOuter))).asInstanceOf[Mono]

    def fromTiling(tiling: Tiling): Mono = fromSides(tiling.edges.toOuter)

  }

}
