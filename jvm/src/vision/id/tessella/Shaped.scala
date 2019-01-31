package vision.id.tessella

import scala.collection.Set
import scala.language.{higherKinds, postfixOps}
import scala.util.Success

import com.typesafe.scalalogging.{LazyLogging, Logger}

import scalax.collection.GraphPredef._
import scalax.collection.constrained.PreCheckFollowUp._
import scalax.collection.constrained._

import vision.id.tessella.Tessella.Tiling

/** Ensures that the underlying `Graph` is a valid tessellation.
  */
class Shaped[N, E[X] <: EdgeLikeIn[X]](override val self: Graph[N, E])
    extends Constraint[N, E](self)
    with NodeChecks[N, E]
    with OptionUtils
    with LazyLogging
    with TilingUtils {

  // ------------- logging -------------

  override lazy val logger = Logger("SHAPED")

  def lazyInfo(s: String): Unit = logger.whenInfoEnabled(logger.info(s))

  def lazyDebug(s: String): Unit = logger.whenDebugEnabled(logger.debug(s))

  def lazyTrace(s: String): Unit = logger.whenTraceEnabled(logger.trace(s))

  // ------------- subclass for carrying over info  -------------

  protected class ShapedResult(followUp: PreCheckFollowUp,
                               val positiveChecked: Boolean,
                               val gapChecked: Boolean,
                               val periRecalc: Boolean,
                               val oldPerimeterEdges: List[Side[Int]])
      extends PreCheckResult(followUp)

  protected object ShapedResult extends PreCheckResultCompanion {
    def apply(followUp: PreCheckFollowUp) =
      new ShapedResult(followUp, false, false, periRecalc = false, Nil)
    def apply(followUp: PreCheckFollowUp,
              positiveChecked: Boolean,
              gapChecked: Boolean,
              periRecalc: Boolean,
              oldPerimeterEdges: List[Side[Int]]) =
      new ShapedResult(followUp, positiveChecked, gapChecked, periRecalc, oldPerimeterEdges)
  }

  // ------------- checks -------------

  private def refusal(msg: String, isAddition: Boolean = true): Nothing =
    throw new IllegalArgumentException((if (isAddition) "Addition" else "Subtraction") + " refused: " + msg)

  /** Skips this pre-check to rely on the post-check `postAdd` except for trivial cases. */
  override def preCreate(nodes: Traversable[N], edges: Traversable[E[N]]): PreCheckResult = {
    lazyInfo("Starting pre-create")
    lazyDebug("nodes: " + nodes.toString)
    lazyDebug("edges" + edges.toString)

    PreCheckResult(PostCheck)
  }

  /** Adding one node can never result into a valid tessellation. */
  override def preAdd(node: N): PreCheckResult = {
    lazyInfo("Starting pre-add single node " + node.toString)

    refusal("cannot add single node " + node.toString)
  }

  private def attachedToPerimeter(end1: Int, end2: Int): Boolean = {
    val periNodes = self.asInstanceOf[Tiling].perimeterOrderedNodes
    periNodes.contains(end1) && periNodes.contains(end2)
  }

  private def attachedToSinglePgon(end1: Int, end2: Int): Boolean =
    self.asInstanceOf[Tiling].getOnePolygonPerimeterPath(end1, end2).isDefined

  private def selfPathEdges(end1: Int, end2: Int, onPerimeter: Boolean = true): Traversable[Side[Int]] =
    self.asInstanceOf[Tiling].getShortestPerimeterPath(end1, end2, onPerimeter)

  private def checkBothOnPerimeter(end1: Int, end2: Int, isAddition: Boolean = true): Unit =
    if (!attachedToPerimeter(end1, end2))
      refusal("endpoints of single edge " + end1 + "~" + end2 + " must be both on perimeter", isAddition)

  /**
    * Single edge must be attached to perimeter
    * There is a special case where the perimeter can be calculated
    *
    * @todo check why post add is run twice
    * */
  override def preAdd(edge: E[N]): ShapedResult = {
    lazyInfo("Starting pre-add single edge " + edge.toString)

    val (end1, end2) = (edge._n(0).asInstanceOf[Int], edge._n(1).asInstanceOf[Int])

    checkBothOnPerimeter(end1, end2)

    val oldPerimeterEdges =
      if (attachedToSinglePgon(end1, end2))
        Nil
      else {
        // no need to recalculate perimeter
        edge.asInstanceOf[Side[Int]].isPerimeter = Some(true)
        selfPathEdges(end1, end2).toList
      }

    ShapedResult(PostCheck, positiveChecked = true, gapChecked = false, periRecalc = false, oldPerimeterEdges)
  }

  /**
    * If edges form a single path, this must be attached to perimeter
    * In this case, there is a nested special case where the perimeter can be calculated
    * and nested inside another more special case where the gaps need not be checked
    * */
  override def preAdd(elems: InParam[N, E]*): ShapedResult = {
    lazyInfo("Starting pre-add elements check")
    lazyDebug("elems " + elems.toString)

    val nodes: Seq[Int] = elems.filter(_.isNode).map(_.asInstanceOf[Int])
    lazyTrace("> Nodes being added: " + nodes.toString)

    if (nodes.exists(_ <= 0))
      refusal("added non positive nodes = " + nodes.filter(_ <= 0))

    val edges: Seq[Side[Int]] = elems.filter(_.isEdge).map(_.asInstanceOf[Side[Int]])
    lazyTrace("> Edges being added: " + edges.toString)

    if (edges.exists(_.toList.exists(_ <= 0)))
      refusal("added non positive edges = " + edges.filter(_.toList.exists(_ <= 0)))

    if (nodes.diff(edges.flatMap(_.toList)).nonEmpty)
      refusal("added 1-degree nodes = " + nodes.diff(edges.flatMap(_.toList)))

    val (gapChecked, oldPerimeterEdges): (Boolean, List[Side[Int]]) = pathEndPoints(edges.toSet) match {
      // if the new edges form a path with two endpoints
      case Success((end1, end2)) =>
        if (!attachedToPerimeter(end1, end2))
          refusal("endpoints of edges path must be both on perimeter " + edges.toString)
        else {
          // find old perimeter edges to be passed to postCheck
          val oldEdges = selfPathEdges(end1, end2)
          lazyTrace(oldEdges.toString)
          if (oldEdges.size != 1 && attachedToSinglePgon(end1, end2))
            refusal("endpoints of edges connecting 'from the inside' of the perimeter " + edges.toString)
          else {
            // new edges changed to perimeter true
            edges.foreach(_.isPerimeter = Some(true))
            (oldEdges.size == 1, oldEdges.toList)
          }
        }
      case _ => (false, Nil)
    }

    ShapedResult(PostCheck, positiveChecked = true, gapChecked, periRecalc = false, oldPerimeterEdges)
  }

  private def checkGap(newGraph: Graph[N, E], preCheck: PreCheckResult, isAddition: Boolean = true): Unit =
    preCheck match {
      case r: ShapedResult if r.gapChecked =>
      case _ =>
        lazyDebug("> checking gap")
        if (newGraph.asInstanceOf[Tiling].hasGap)
          refusal("tiling with gap", isAddition)
    }

  /** Check the whole `newGraph`. */
  override def postAdd(newGraph: Graph[N, E],
                       passedNodes: Traversable[N],
                       passedEdges: Traversable[E[N]],
                       preCheck: PreCheckResult): Boolean = {
    lazyInfo("Starting post-add")
    lazyDebug("newGraph " + newGraph)
    lazyDebug("passedNodes " + passedNodes)
    lazyDebug("passedEdges " + passedEdges)

    preCheck match {
      case r: ShapedResult if r.positiveChecked =>
      case _ =>
        lazyDebug("> checking positive values")
        if (!newGraph.hasPositiveValues)
          refusal("non positive nodes = " + newGraph.nodes.filter(_.toOuter match {
            case i: Int => i <= 0
            case _      => false
          }))
    }
    lazyDebug("> checking nodes' degree")
    if (!newGraph.hasRegularNodes)
      refusal(
        "nodes with wrong number of edges = " +
          newGraph.nodes.filter(node => node.degree < 2 || node.degree > 6))
    preCheck match {
      case r: ShapedResult if r.oldPerimeterEdges.nonEmpty =>
        newGraph.edges
          .foreach(edge =>
            if (r.oldPerimeterEdges.contains(edge.toOuter)) edge.asInstanceOf[Tiling#EdgeT].isPerimeter = Some(false))
      case _ =>
        lazyDebug("> checking connected")
        if (!newGraph.isConnected)
          refusal("graph not connected")
    }
    if (!newGraph.asInstanceOf[Tiling].hasPerimeterSet) {
      lazyDebug("> computing perimeter")

      if (newGraph.asInstanceOf[Tiling].setPerimeter.isFailure)
        refusal("could not build perimeter")
    }
    lazyDebug("> checking perimeter polygon")
    if (newGraph.asInstanceOf[Tiling].toPerimeterSimplePolygon.isFailure) {
      refusal("perimeter is not a simple polygon")
    }
    checkGap(newGraph, preCheck)
    true
  }

  /**
    * Ok if the node is at the center of an hexagon by 6 triangles
    * or at the center of a valid perimeter p-gon
    * Not ok if otherwise not on the perimeter
    * or if on the perimeter but leaving open perimeter edges
    * In all other cases needs a post check for the perimeter polygon
    * */
  override def preSubtract(node: self.NodeT, forced: Boolean): ShapedResult = {
    lazyInfo("Starting pre-subtract node " + node.toString)
    lazyDebug("forced " + forced)
    lazyDebug("self " + self)

    if (node.degree == 6) {
      ShapedResult(Complete)
    } else {
      val periNodes = self.asInstanceOf[Tiling].perimeterOrderedNodes.init
      node.toOuter.asInstanceOf[Int] match {
        case n if periNodes.contains(n) =>
          if (node.neighbors.exists(_.degree == 2))
            refusal("non removable perimeter node " + node.toString, isAddition = false)
          else {
            // find existing non perimeter edges and transform to perimeter
            val (end1, end2) = periNodes.circularNeighborsOf(n).safeGet()
            val newPerimeterEdges = selfPathEdges(end1, end2, onPerimeter = false).toList
            ShapedResult(PostCheck, positiveChecked = true, gapChecked = true, periRecalc = false, newPerimeterEdges)
          }
        case _ =>
          if (RegPgon.edgesNumberToPgon.get(periNodes.length).isDefined &&
              periNodes.diff(self.nodes.filterNot(_ == node).toList.map(_.toOuter.asInstanceOf[Int])).isEmpty)
            ShapedResult(Complete)
          else
            refusal("non removable non perimeter node " + node.toString, isAddition = false)
      }
    }
  }

  /**
    * Not ok if not connecting two perimeter nodes
    * Also not ok if perimeter leaving open other perimeter edges
    * In all other cases needs a post check for the perimeter polygon
    * */
  override def preSubtract(edge: self.EdgeT, simple: Boolean): ShapedResult = {
    lazyInfo("Starting pre-subtract edge " + edge.toString)
    lazyDebug("simple " + simple)

    val (end1, end2) = (edge._n(0).toOuter.asInstanceOf[Int], edge._n(1).toOuter.asInstanceOf[Int])

    checkBothOnPerimeter(end1, end2, isAddition = false)

    val periEdges = self.asInstanceOf[Tiling].perimeterOrderedEdges.init
    val newPerimeterEdges =
      if (periEdges.contains(edge.toOuter.asInstanceOf[Side[Int]])) {
        if (edge.toList.exists(_.degree == 2))
          refusal("perimeter edge " + edge.toString + " has node of degree 2", isAddition = false)
        selfPathEdges(end1, end2, onPerimeter = false).toList
      } else {
        Nil
      }
    ShapedResult(PostCheck, positiveChecked = true, gapChecked = true, periRecalc = false, newPerimeterEdges)
  }

  /**
    * If nodes form a single perimeter path
    * */
  override def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean): ShapedResult = {
    lazyInfo("Starting pre-subtract nodes and edges")
    lazyDebug("nodes " + nodes.toString)
    lazyDebug("edges " + edges.toString)
    lazyDebug("simple " + simple)
    lazyDebug("self  " + self)

    if (self.nodes.length == nodes.size)
      ShapedResult(Complete)
    else {
      if (edges.isEmpty && nodes.forall(_.degree == 2)) {
        val ns        = nodes.toList.map(_.toOuter.asInstanceOf[Int])
        val periEdges = self.asInstanceOf[Tiling].perimeterOrderedEdges.filter(_.toList.intersect(ns).nonEmpty)
        pathEndPoints(periEdges.toSet) match {
          // if the new edges form a path with two endpoints
          case Success((end1, end2)) =>
            if (self.asInstanceOf[Tiling].get(end1).degree > 2 && self.asInstanceOf[Tiling].get(end2).degree > 2) {
              val newPerimeterEdges = selfPathEdges(end1, end2, onPerimeter = false).toList
              return ShapedResult(PostCheck, positiveChecked = true, gapChecked = true, periRecalc = false, newPerimeterEdges)
            } else
              refusal("perimeter nodes form a path adjacent to perimeter node of degree 2", isAddition = false)
          case _ =>
        }
      }
      ShapedResult(PostCheck, positiveChecked = true, gapChecked = false, periRecalc = true, Nil)
    }
  }

  /** Check the whole `newGraph`. */
  override def postSubtract(newGraph: Graph[N, E],
                            passedNodes: Traversable[N],
                            passedEdges: Traversable[E[N]],
                            preCheck: PreCheckResult): Boolean = {
    lazyInfo("Starting post-subtract")
    lazyDebug("newGraph " + newGraph)
    lazyDebug("passedNodes " + passedNodes)
    lazyDebug("passedEdges " + passedEdges)

    lazyDebug("> checking nodes' degree")
    if (!newGraph.hasRegularNodes)
      refusal(
        "nodes with wrong number of edges = " +
          newGraph.nodes.filter(node => node.degree < 2 || node.degree > 6))
    preCheck match {
      case r: ShapedResult if r.gapChecked =>
      case _ =>
        lazyDebug("> checking connected")
        if (!newGraph.isConnected)
          refusal("graph not connected")
    }
    preCheck match {
      case r: ShapedResult if r.oldPerimeterEdges.nonEmpty =>
        newGraph.edges
          .foreach(edge =>
            if (r.oldPerimeterEdges.contains(edge.toOuter)) edge.asInstanceOf[Tiling#EdgeT].isPerimeter = Some(true))
      case _ =>
    }
    preCheck match {
      case r: ShapedResult =>
        if (r.periRecalc || !newGraph.asInstanceOf[Tiling].hasPerimeterSet) {
          lazyDebug("> computing perimeter")
          if (newGraph.asInstanceOf[Tiling].setPerimeter.isFailure)
            refusal("could not build perimeter")
        }
      case _ =>
    }
    lazyDebug("> checking perimeter polygon")
    if (newGraph.asInstanceOf[Tiling].toPerimeterSimplePolygon.isFailure) {
      refusal("perimeter is not a simple polygon")
    }
    checkGap(newGraph, preCheck, isAddition = false)

    true
  }

  override def onAdditionRefused(refusedNodes: Traversable[N],
                                 refusedEdges: Traversable[E[N]],
                                 graph: Graph[N, E]): Boolean =
    refusal("nodes = " + refusedNodes + ", " + "edges = " + refusedEdges)

}

object Shaped extends ConstraintCompanion[Shaped] {
  def apply[N, E[X] <: EdgeLikeIn[X]](self: Graph[N, E]): Shaped[N, E] = new Shaped[N, E](self)
}
