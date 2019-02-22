package vision.id.tessella

import scala.collection.Set
import scala.language.{higherKinds, postfixOps}
import scala.util.{Failure, Success}

import com.typesafe.scalalogging.{LazyLogging, Logger}

import scalax.collection.GraphPredef._
import scalax.collection.constrained.PreCheckFollowUp._
import scalax.collection.constrained._

import vision.id.tessella.Polar.RegularPgon
import vision.id.tessella.Tau.TAU
import vision.id.tessella.Tessella.Tiling

/** Ensures that the underlying `Graph` is a valid tessellation.
  */
class Shaped[N, E[X] <: EdgeLikeIn[X]](override val self: Graph[N, E])
    extends Constraint[N, E](self)
    with NodeChecks[N, E]
    with OptionUtils
    with MathUtils
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
      new ShapedResult(followUp, positiveChecked = false, gapChecked = false, periRecalc = false, Nil)
    def apply(followUp: PreCheckFollowUp,
              positiveChecked: Boolean,
              gapChecked: Boolean,
              periRecalc: Boolean,
              oldPerimeterEdges: List[Side[Int]]) =
      new ShapedResult(followUp, positiveChecked, gapChecked, periRecalc, oldPerimeterEdges)
  }

  // ------------- checks -------------

  /** Skips this pre-check to rely on the post-check `postAdd` except for trivial cases. */
  override def preCreate(nodes: Traversable[N], edges: Traversable[E[N]]): ShapedResult = {
    lazyInfo("Starting pre-create")
    lazyTrace("nodes: " + nodes.toString)
    lazyTrace("edges" + edges.toString)

    ShapedResult(PostCheck)
  }

  private def attachedToPerimeter(end1: Int, end2: Int): Boolean = {
    val periNodes = self.asInstanceOf[Tiling].perimeterOrderedNodes
    periNodes.contains(end1) && periNodes.contains(end2)
  }

  private def selfPathEdges(end1: Int, end2: Int, onPerimeter: Boolean = true): Traversable[Side[Int]] =
    self.asInstanceOf[Tiling].getShortestPerimeterPath(end1, end2, onPerimeter)

  private def endPointsOrderedIndexes(end1: Int, end2: Int): (Int, Int) = {
    val periNodes = self.asInstanceOf[Tiling].perimeterOrderedNodes
    val i1        = periNodes.indexOf(end1)
    val i2        = periNodes.indexOf(end2)
    if (i1 < i2) (i1, i2) else (i2, i1)
  }

  private def angleCircularPathsBetween(first: Int, second: Int): List[List[Double]] = {
    val angles     = self.asInstanceOf[Tiling].toPerimeterSimplePolygon.safeGet.pps.map(_.phi)
    val (a1, temp) = angles.splitAt(first + 1)
    val (b, a2)    = temp.splitAt(second - (first + 1))
    List(a1.init ++ a2.tail, b)
  }

  private def isPolygonPath(end1: Int, end2: Int, edgesSize: Int): Boolean = {
    val (first, second) = endPointsOrderedIndexes(end1, end2)

    angleCircularPathsBetween(first, second).exists({
      case angle :: angles =>
        angles.forall(_ ~= angle) && (RegularPgon.edgesNumberFrom(angle - (TAU / 2)) match {
          case Success(edgesNumber) => edgesNumber - (angles.size + 2) - edgesSize == 0
          case Failure(_)           => false
        })
      case Nil => false
    })
  }

  private def isSquareDiagonal(end1: Int, end2: Int): Boolean = {
    val neigh1 = (self get end1.asInstanceOf[N]).neighbors.toList
    val neigh2 = (self get end2.asInstanceOf[N]).neighbors.toList
    val shared = neigh1.intersect(neigh2)
    def onPeri: List[Int] =
      shared.map(_.toOuter.asInstanceOf[Int]).intersect(self.asInstanceOf[Tiling].perimeterOrderedNodes)
    shared.size == 2 && onPeri.size == 2
  }

  // ------------- addition -------------

  private def goToPostAdd(newPerimeterEdges: Seq[Side[Int]], oldPerimeterEdges: List[Side[Int]]): ShapedResult = {
    // new edges changed to perimeter true
    newPerimeterEdges.foreach(_.isPerimeter = Some(true))
    ShapedResult(PostCheck, positiveChecked = true, gapChecked = true, periRecalc = false, oldPerimeterEdges)
  }

  /** Adding one node can never result into a valid tessellation. */
  override def preAdd(node: N): ShapedResult = {
    lazyInfo("Starting pre-add single node " + node.toString)

    val isContained = self contains node
    lazyDebug("> node contained: " + isContained)
    if (isContained) ShapedResult(Complete)
    else ShapedResult(Abort)
  }

  /**
    * Single edge must be attached to perimeter
    * There is a special case where the perimeter can be calculated
    */
  override def preAdd(edge: E[N]): ShapedResult = {
    lazyInfo("Starting pre-add single edge " + edge.toString)

    val (end1, end2) = (edge._n(0).asInstanceOf[Int], edge._n(1).asInstanceOf[Int])

    if (!attachedToPerimeter(end1, end2)) {
      lazyDebug("> endpoints of single edge " + end1 + "~" + end2 + " must be both on perimeter")
      ShapedResult(Abort)
    } else if (isPolygonPath(end1, end2, 1))
      goToPostAdd(Seq(edge.asInstanceOf[Side[Int]]), selfPathEdges(end1, end2).toList)
    else if (isSquareDiagonal(end1, end2))
      ShapedResult(PostCheck, positiveChecked = true, gapChecked = false, periRecalc = false, Nil)
    else
      ShapedResult(Abort)
  }

  /**
    * If edges form a single path, this must be attached to perimeter
    * In this case, there is a nested special case where the perimeter can be calculated
    * and nested inside another more special case where the gaps need not be checked
    */
  override def preAdd(elems: InParam[N, E]*): ShapedResult = {
    lazyInfo("Starting pre-add elements check")
    lazyDebug("elems: " + elems.toString)

    val nodes: Seq[Int] = elems.filter(_.isNode).map(_.asInstanceOf[Int])
    lazyTrace("> Nodes being added: " + nodes.toString)

    if (nodes.exists(_ <= 0)) {
      lazyDebug("> added non positive nodes = " + nodes.filter(_ <= 0))
      ShapedResult(Abort)
    } else {
      val edges: Seq[Side[Int]] = elems.filter(_.isEdge).map(_.asInstanceOf[Side[Int]])
      lazyTrace("> Edges being added: " + edges.toString)

      if (edges.exists(_.toList.exists(_ <= 0))) {
        lazyDebug("> added non positive edges = " + edges.filter(_.toList.exists(_ <= 0)))
        ShapedResult(Abort)
      } else if (nodes.diff(edges.flatMap(_.toList)).nonEmpty) {
        lazyDebug("> added 1-degree nodes = " + nodes.diff(edges.flatMap(_.toList)))
        ShapedResult(Abort)
      } else {
        pathEndPoints(edges.toSet) match {
          // if the new edges form a path with two endpoints
          case Success((end1, end2)) =>
            if (!attachedToPerimeter(end1, end2)) {
              lazyDebug("> endpoints of edges path must be both on perimeter " + edges.toString)
              ShapedResult(Abort)
            } else {
              // find old perimeter edges to be passed to postCheck
              val oldEdges = selfPathEdges(end1, end2)
              lazyTrace(oldEdges.toString)

              if (oldEdges.size == 1 || isPolygonPath(end1, end2, edges.size))
                goToPostAdd(edges, oldEdges.toList)
              else ShapedResult(Abort)
            }
          case _ =>
            ShapedResult(PostCheck, positiveChecked = true, gapChecked = false, periRecalc = false, Nil)
        }
      }
    }
  }

  private def setEdges(newGraph: Graph[N, E],
                       preCheck: PreCheckResult,
                       isPerimeter: Option[Boolean] = Some(true)): Unit =
    preCheck match {
      case r: ShapedResult if r.oldPerimeterEdges.nonEmpty =>
        newGraph.edges
          .intersect(r.oldPerimeterEdges map (old => newGraph get old.asInstanceOf[E[N]]))
          .foreach(_.asInstanceOf[Tiling#EdgeT].isPerimeter = isPerimeter)
      case _ =>
    }

  private def hasNoGaps(newGraph: Graph[N, E], preCheck: PreCheckResult): Boolean =
    preCheck match {
      case r: ShapedResult if r.gapChecked => true
      case _ =>
        lazyDebug("> checking gap")
        !newGraph.asInstanceOf[Tiling].hasGap
    }

  /** Check the whole `newGraph`. */
  override def postAdd(newGraph: Graph[N, E],
                       passedNodes: Traversable[N],
                       passedEdges: Traversable[E[N]],
                       preCheck: PreCheckResult): Boolean = {
    lazyInfo("Starting post-add")
    lazyDebug("newGraph: " + newGraph)
    lazyDebug("passedNodes: " + passedNodes)
    lazyDebug("passedEdges: " + passedEdges)
    lazyTrace("preCheck: " + (preCheck match {
      case r: ShapedResult =>
        r.positiveChecked + " " + r.gapChecked + " " + r.periRecalc + " " + r.oldPerimeterEdges
      case _ => "no ShapedResult"
    }))

    def positiveCheck: Boolean =
      preCheck match {
        case r: ShapedResult if r.positiveChecked => true
        case _ =>
          lazyDebug("> checking positive values")
          newGraph.hasPositiveValues
      }

    def isConnected: Boolean = {
      setEdges(newGraph, preCheck, Some(false))
      preCheck match {
        case r: ShapedResult if r.oldPerimeterEdges.nonEmpty =>
          true
        case _ =>
          lazyDebug("> checking connected")
          newGraph.isConnected
      }
    }

    def hasPerimeter: Boolean =
      if (!newGraph.asInstanceOf[Tiling].hasPerimeterSet) {
        lazyDebug("> computing perimeter")
        newGraph.asInstanceOf[Tiling].setPerimeter.isSuccess
      } else true

    def polygonOk: Boolean = {
      lazyDebug("> checking perimeter polygon")
      newGraph.asInstanceOf[Tiling].toPerimeterSimplePolygon.isSuccess
    }

    positiveCheck && newGraph.hasRegularNodes && isConnected && hasPerimeter && polygonOk && hasNoGaps(newGraph,
                                                                                                       preCheck)

  }

  override def onAdditionRefused(refusedNodes: Traversable[N],
                                 refusedEdges: Traversable[E[N]],
                                 graph: Graph[N, E]): Boolean = {
    lazyInfo("Starting addition refused handle")
    lazyDebug("refusedNodes: " + refusedNodes.toString)
    lazyDebug("refusedEdges: " + refusedEdges.toString)
    lazyDebug("graph: " + graph.toString)
    throw new IllegalArgumentException(
      "Addition refused: " +
        "nodes = " + refusedNodes + ", " +
        "edges = " + refusedEdges)
  }

  // ------------- subtraction -------------

  def checkNewPerimeter(end1: Int, end2: Int): ShapedResult =
    ShapedResult(PostCheck,
                 positiveChecked = true,
                 gapChecked = true,
                 periRecalc = false,
                 selfPathEdges(end1, end2, onPerimeter = false).toList)

  /**
    * Ok if the node is at the center of an hexagon by 6 triangles
    * or at the center of a valid perimeter p-gon
    * Not ok if otherwise not on the perimeter
    * or if on the perimeter but leaving open perimeter edges
    * In all other cases needs a post check for the perimeter polygon
    */
  override def preSubtract(node: self.NodeT, forced: Boolean): ShapedResult = {
    lazyInfo("Starting pre-subtract node " + node.toString)
    lazyDebug("forced: " + forced)
    lazyDebug("self: " + self)

    if (node.degree == 6) {
      lazyDebug("> degree 6")
      ShapedResult(Complete)
    } else {
      lazyDebug("> degree < 6")
      val periNodes = self.asInstanceOf[Tiling].perimeterOrderedNodes.init
      node.toOuter.asInstanceOf[Int] match {
        case n if periNodes.contains(n) =>
          if (node.neighbors.exists(_.degree == 2)) {
            lazyDebug("> non removable perimeter node 1")
            ShapedResult(Abort)
          } else {
            // find existing non perimeter edges and transform to perimeter
            lazyDebug("> found existing edges")
            val (end1, end2) = periNodes.circularNeighborsOf(n).safeGet()
            checkNewPerimeter(end1, end2)
          }
        case _ =>
          if (RegPgon.edgesNumberToPgon.get(periNodes.length).isDefined &&
              periNodes.diff(self.nodes.filterNot(_ == node).toList.map(_.toOuter.asInstanceOf[Int])).isEmpty)
            ShapedResult(Complete)
          else {
            lazyDebug("> non removable perimeter node 2")
            ShapedResult(Abort)
          }
      }
    }
  }

  /**
    * Not ok if not connecting two perimeter nodes
    * Also not ok if perimeter leaving open other perimeter edges
    * In all other cases needs a post check for the perimeter polygon
    */
  override def preSubtract(edge: self.EdgeT, simple: Boolean): ShapedResult = {
    lazyInfo("Starting pre-subtract edge " + edge.toString)
    lazyDebug("simple " + simple)

    val (end1, end2) = (edge._n(0).toOuter.asInstanceOf[Int], edge._n(1).toOuter.asInstanceOf[Int])

    if (!attachedToPerimeter(end1, end2)) {
      lazyDebug("> endpoints of single edge " + end1 + "~" + end2 + " must be both on perimeter")
      ShapedResult(Abort)
    } else {
      val periEdges = self.asInstanceOf[Tiling].perimeterOrderedEdges.init
      if (periEdges.contains(edge.toOuter.asInstanceOf[Side[Int]])) {
        if (edge.toList.exists(_.degree == 2)) {
          lazyDebug("> perimeter edge " + edge.toString + " has node of degree 2")
          ShapedResult(Abort)
        } else
          checkNewPerimeter(end1, end2)
      } else {
        ShapedResult(PostCheck, positiveChecked = true, gapChecked = true, periRecalc = false, Nil)
      }
    }
  }

  /**
    * If nodes form a single perimeter path
    */
  override def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean): ShapedResult = {
    lazyInfo("Starting pre-subtract nodes and edges")
    lazyDebug("nodes: " + nodes.toString)
    lazyDebug("edges: " + edges.toString)
    lazyDebug("simple: " + simple)
    lazyTrace("self: " + self)

    def checkRecalc: ShapedResult =
      ShapedResult(PostCheck, positiveChecked = true, gapChecked = false, periRecalc = true, Nil)

    nodes match {
      case all if all.size == self.nodes.length => ShapedResult(Complete)
      case peri if edges.isEmpty && peri.forall(_.degree == 2) =>
        val ns        = nodes.toList.map(_.toOuter.asInstanceOf[Int])
        val periEdges = self.asInstanceOf[Tiling].perimeterOrderedEdges.filter(_.toList.intersect(ns).nonEmpty)
        pathEndPoints(periEdges.toSet) match {
          // if the new edges form a path with two endpoints
          case Success((end1, end2)) =>
            if (self.asInstanceOf[Tiling].get(end1).degree > 2 && self.asInstanceOf[Tiling].get(end2).degree > 2) {
              checkNewPerimeter(end1, end2)
            } else {
              lazyDebug("> perimeter nodes form a path adjacent to perimeter node of degree 2")
              ShapedResult(Abort)
            }
          case _ => checkRecalc
        }
      case _ => checkRecalc
    }
  }

  /** Check the whole `newGraph`. */
  override def postSubtract(newGraph: Graph[N, E],
                            passedNodes: Traversable[N],
                            passedEdges: Traversable[E[N]],
                            preCheck: PreCheckResult): Boolean = {
    lazyInfo("Starting post-subtract")
    lazyDebug("newGraph: " + newGraph)
    lazyDebug("passedNodes: " + passedNodes)
    lazyDebug("passedEdges: " + passedEdges)
    lazyTrace("preCheck: " + (preCheck match {
      case r: ShapedResult =>
        r.positiveChecked + " " + r.gapChecked + " " + r.periRecalc + " " + r.oldPerimeterEdges
      case _ => ""
    }))

    lazyDebug("> checking nodes' degree")

    def isConnected: Boolean =
      preCheck match {
        case r: ShapedResult if r.gapChecked => true
        case _ =>
          lazyDebug("> checking connected")
          newGraph.isConnected
      }

    def hasPerimeter: Boolean = {
      setEdges(newGraph, preCheck)
      preCheck match {
        case r: ShapedResult if r.periRecalc || !newGraph.asInstanceOf[Tiling].hasPerimeterSet =>
          lazyDebug("> computing perimeter")
          newGraph.asInstanceOf[Tiling].setPerimeter.isSuccess
        case _ => true
      }
    }

    def polygonOk: Boolean = {
      lazyDebug("> checking perimeter polygon")
      newGraph.asInstanceOf[Tiling].toPerimeterSimplePolygon.isSuccess
    }

    val isValid = newGraph.hasRegularNodes && isConnected && hasPerimeter && polygonOk && hasNoGaps(newGraph, preCheck)
    if (!isValid) setEdges(newGraph, preCheck, isPerimeter = Some(false))
    isValid
  }

  override def onSubtractionRefused(refusedNodes: Traversable[Graph[N, E]#NodeT],
                                    refusedEdges: Traversable[Graph[N, E]#EdgeT],
                                    graph: Graph[N, E]): Boolean = {
    lazyInfo("Starting subtraction refused handle")
    lazyDebug("refusedNodes: " + refusedNodes.toString)
    lazyDebug("refusedEdges: " + refusedEdges.toString)
    lazyDebug("graph: " + graph.toString)
    throw new IllegalArgumentException(
      "Subtraction refused: " +
        "nodes = " + refusedNodes + ", " +
        "edges = " + refusedEdges)
  }
}

object Shaped extends ConstraintCompanion[Shaped] {
  def apply[N, E[X] <: EdgeLikeIn[X]](self: Graph[N, E]): Shaped[N, E] = new Shaped[N, E](self)
}
