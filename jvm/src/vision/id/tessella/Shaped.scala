package vision.id.tessella

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.constrained.PreCheckFollowUp._
import scalax.collection.constrained._
import vision.id.tessella.Polar.{PointPolar, UnitSimplePgon}
import vision.id.tessella.Tau.τ

import scala.collection.Set
import scala.language.{higherKinds, postfixOps}
import scala.util.Try

/** Ensures that the underlying `Graph` is connected if it is undirected
  * or weakly connected if it is directed.
  */
class Shaped[N, E[X] <: EdgeLikeIn[X]](override val self: Graph[N, E])
    extends Constraint[N, E](self)
    with NodeChecks[N, E]
    with Perimeter
    with Neighbors {

  /** Skips this pre-check to rely on the post-check `postAdd` except for trivial cases. */
  override def preCreate(nodes: Traversable[N], edges: Traversable[E[N]]) =
    PreCheckResult(PostCheck)

  /** Adding one node can never result into a valid tessellation.  */
  override def preAdd(node: N): PreCheckResult =
    PreCheckResult(Abort)

  /** Only in very rare cases (to be analyzed) adding one edge can result into a valid tessellation. */
  override def preAdd(edge: E[N]): PreCheckResult =
    PreCheckResult(Abort)

  /** To be analyzed, for the time being skip */
  override def preAdd(elems: InParam[N, E]*): PreCheckResult =
    PreCheckResult(PostCheck)

  private def getSimplePolygon(g: Graph[Int, UnDiEdge]): Try[UnitSimplePgon] = Try {
    val (periNodes, _): (List[Int], List[UnDiEdge[Int]]) = g.perimeterNodesEdges

    val (_, periPaths): (List[List[Int]], List[List[List[Int]]]) =
      periNodes.init.map(g.outerNodeNeighbors(_, periNodes).unzip).unzip

    val vertexes: List[Vertex] = periPaths.map(paths ⇒ Vertex.p(paths.init.map(_.size + 2)))

    // implies satisfying requirements of UnitSimplePgon
    new UnitSimplePgon(vertexes.map(v ⇒ new PointPolar(1.0, τ / 2 - v.α)))

  }

  /** Check the whole `newGraph`. */
  override def postAdd(newGraph: Graph[N, E],
                       passedNodes: Traversable[N],
                       passedEdges: Traversable[E[N]],
                       preCheck: PreCheckResult): Boolean = {
    newGraph.hasPositiveValues &&
    newGraph.hasRegularNodes &&
    newGraph.isConnected &&
    getSimplePolygon(newGraph.asInstanceOf[Graph[Int, UnDiEdge]]).isSuccess
  }

  /** Only in very rare cases (to be analyzed) subtracting one node (and related edges) can result into a valid tessellation. */
  override def preSubtract(node: self.NodeT, forced: Boolean): PreCheckResult =
    PreCheckResult(Abort)

  /** Only in very rare cases (to be analyzed) subtracting one edge can result into a valid tessellation. */
  override def preSubtract(edge: self.EdgeT, simple: Boolean): PreCheckResult =
    PreCheckResult(Abort)

  /** To be analyzed, for the time being skip */
  override def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean): PreCheckResult =
    PreCheckResult(PostCheck)

  override def onAdditionRefused(refusedNodes: Traversable[N],
                                 refusedEdges: Traversable[E[N]],
                                 graph: Graph[N, E]): Boolean =
    throw new IllegalArgumentException(
      "Addition refused: " +
        "nodes = " + refusedNodes + ", " +
        "edges = " + refusedEdges)

}

object Shaped extends ConstraintCompanion[Shaped] {
  def apply[N, E[X] <: EdgeLikeIn[X]](self: Graph[N, E]): Shaped[N, E] = new Shaped[N, E](self)
}
