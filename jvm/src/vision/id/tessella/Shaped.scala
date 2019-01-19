package vision.id.tessella

import scala.collection.Set
import scala.language.{higherKinds, postfixOps}
import scala.util.Success

import scalax.collection.GraphPredef._
import scalax.collection.constrained.PreCheckFollowUp._
import scalax.collection.constrained._

import vision.id.tessella.Alias.Tiling

/** Ensures that the underlying `Graph` is a valid tessellation.
  */
class Shaped[N, E[X] <: EdgeLikeIn[X]](override val self: Graph[N, E])
    extends Constraint[N, E](self)
    with NodeChecks[N, E]
    with OptionUtils
    with TilingUtils {

  private def refusal(msg: String): Nothing =
    throw new IllegalArgumentException("Addition refused: " + msg)

  /** Skips this pre-check to rely on the post-check `postAdd` except for trivial cases. */
  override def preCreate(nodes: Traversable[N], edges: Traversable[E[N]]) =
    PreCheckResult(PostCheck)

  /** Adding one node can never result into a valid tessellation. */
  override def preAdd(node: N): PreCheckResult =
    refusal("cannot add a single node")

  /** Only in very rare cases (to be analyzed) adding one edge can result into a valid tessellation. */
  override def preAdd(edge: E[N]): PreCheckResult =
    PreCheckResult(PostCheck)

  protected class Result(followUp: PreCheckFollowUp,
                         val positiveChecked: Boolean,
                         val gapChecked: Boolean,
                         val oldPerimeterEdges: List[Side[Int]])
      extends PreCheckResult(followUp)

  protected object Result extends PreCheckResultCompanion {
    def apply(followUp: PreCheckFollowUp) =
      new Result(followUp, false, false, Nil)
    def apply(followUp: PreCheckFollowUp,
              positiveChecked: Boolean,
              gapChecked: Boolean,
              oldPerimeterEdges: List[Side[Int]]) =
      new Result(followUp, positiveChecked, gapChecked, oldPerimeterEdges)
  }

  /**
    * Non positive nodes checked here and not post-checked
    *
    * Gaps not post-checked if:
    *   - edges form a path
    *   - starting from a perimeter node
    *   - ending in a neighbor perimeter node
    * */
  override def preAdd(elems: InParam[N, E]*): Result = {
    val nodes = elems.filter(_.isNode).map(_.asInstanceOf[Int])
    if (nodes.exists(_ <= 0))
      refusal("added non positive nodes = " + nodes.filter(_ <= 0))
    val edges = elems.filter(_.isEdge).map(_.asInstanceOf[Side[Int]])
    if (edges.exists(_.toList.exists(_ <= 0)))
      refusal("added non positive edges = " + edges.filter(_.toList.exists(_ <= 0)))

    val periNodes = self.asInstanceOf[Tiling].perimeterOrderedNodes.init

    val (gapChecked, oldPerimeterEdges): (Boolean, List[Side[Int]]) = pathEndPoints(edges.toSet) match {
      // if the new edges form a path with two endpoints
      case Success((end1, end2)) =>
        // if attached to the perimeter
        if (periNodes.contains(end1) && periNodes.contains(end2)) {
          // new edges changed to perimeter true
          edges.foreach(_.isPerimeter = Some(true))
          // find old perimeter edges to be passed to postCheck
          val n1 = self.nodes.find(_.toOuter == end1).safeGet()
          val n2 = self.nodes.find(_.toOuter == end2).safeGet()
          val oldEdges =
            (n1.withSubgraph(edges = _.asInstanceOf[Tiling#EdgeT].isPerimeter.contains(true)) shortestPathTo n2)
              .safeGet()
              .edges
          (oldEdges.size == 1, oldEdges.toList.map(_.toOuter.asInstanceOf[Side[Int]]))
        } else (false, Nil)
      case _ => (false, Nil)
    }

    Result(PostCheck, positiveChecked = true, gapChecked, oldPerimeterEdges)
  }

  /** Check the whole `newGraph`. */
  override def postAdd(newGraph: Graph[N, E],
                       passedNodes: Traversable[N],
                       passedEdges: Traversable[E[N]],
                       preCheck: PreCheckResult): Boolean = {
    preCheck match {
      case r: Result if r.positiveChecked =>
      case _ =>
        if (!newGraph.hasPositiveValues)
          refusal("non positive nodes = " + newGraph.nodes.filter(_.toOuter match {
            case i: Int => i <= 0
            case _      => false
          }))
    }
    if (!newGraph.hasRegularNodes)
      refusal(
        "nodes with wrong number of edges = " +
          newGraph.nodes.filter(node => node.degree < 2 || node.degree > 6))
    preCheck match {
      case r: Result if r.oldPerimeterEdges.nonEmpty =>
        newGraph.edges
          .foreach(edge =>
            if (r.oldPerimeterEdges.contains(edge.toOuter)) edge.asInstanceOf[Tiling#EdgeT].isPerimeter = Some(false))
      case _ =>
        if (!newGraph.isConnected)
          refusal("graph not connected")
    }
    if (!newGraph.asInstanceOf[Tiling].hasPerimeterSet && newGraph.asInstanceOf[Tiling].setPerimeter.isFailure)
      refusal("could not build perimeter")
    if (newGraph.asInstanceOf[Tiling].toPerimeterSimplePolygon.isFailure) {
      refusal("perimeter is not a simple polygon")
    }
    preCheck match {
      case r: Result if r.gapChecked =>
      case _ =>
        if (newGraph.asInstanceOf[Tiling].hasGap)
          refusal("tiling with gap")
    }
    true
  }

  /** Only in very rare cases (to be analyzed) subtracting one node (and related edges)
    * can result into a valid tessellation. */
  override def preSubtract(node: self.NodeT, forced: Boolean): PreCheckResult =
    PreCheckResult(PostCheck)

  /** Only in very rare cases (to be analyzed) subtracting one edge can result into a valid tessellation. */
  override def preSubtract(edge: self.EdgeT, simple: Boolean): PreCheckResult =
    PreCheckResult(PostCheck)

  /** To be analyzed, for the time being skip */
  override def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean): PreCheckResult =
    PreCheckResult(PostCheck)

  override def onAdditionRefused(refusedNodes: Traversable[N],
                                 refusedEdges: Traversable[E[N]],
                                 graph: Graph[N, E]): Boolean =
    refusal("nodes = " + refusedNodes + ", " + "edges = " + refusedEdges)

}

object Shaped extends ConstraintCompanion[Shaped] {
  def apply[N, E[X] <: EdgeLikeIn[X]](self: Graph[N, E]): Shaped[N, E] = new Shaped[N, E](self)
}
