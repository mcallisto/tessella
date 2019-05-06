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
class ByRegularPgons[N, E[X] <: EdgeLikeIn[X]](override val self: Graph[N, E])
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

  protected class Result(followUp: PreCheckFollowUp,
                         val action: Boolean = false,
                         val gapChecked: Boolean = false,
                         val perimeterEdges: List[Side[Int]] = Nil)
      extends PreCheckResult(followUp)

  protected object Result extends PreCheckResultCompanion {
    def apply(followUp: PreCheckFollowUp) =
      new Result(followUp, action = false, gapChecked = false, perimeterEdges = Nil)
    def apply(followUp: PreCheckFollowUp,
              action: Boolean = false,
              gapChecked: Boolean = false,
              perimeterEdges: List[Side[Int]] = Nil) =
      new Result(followUp, action, gapChecked, perimeterEdges)
  }

  // ------------- checks -------------

  /** Skips this pre-check to rely on the post-check `postAdd` except for trivial cases. */
  override def preCreate(nodes: Traversable[N], edges: Traversable[E[N]]): PreCheckResult = {
    lazyInfo("Starting pre-create")
    lazyTrace("nodes: " + nodes.toString)
    lazyTrace("edges" + edges.toString)

    PreCheckResult(PostCheck)
  }

  protected def attachedToPerimeter(end1: Int, end2: Int): Boolean = {
    val periNodes = self.asInstanceOf[Tiling].perimeterOrderedNodes
    periNodes.contains(end1) && periNodes.contains(end2)
  }

  protected def selfPathEdges(end1: Int, end2: Int, onPerimeter: Boolean = true): Traversable[Side[Int]] =
    self.asInstanceOf[Tiling].getShortestPerimeterPath(end1, end2, onPerimeter)

  protected def endPointsOrderedIndexes(end1: Int, end2: Int): (Int, Int) = {
    val periNodes = self.asInstanceOf[Tiling].perimeterOrderedNodes
    val i1        = periNodes.indexOf(end1)
    val i2        = periNodes.indexOf(end2)
    if (i1 < i2) (i1, i2) else (i2, i1)
  }

  protected def findPathFitting(paths: List[List[Double]], edgesSize: Int): Option[List[Double]] =
    paths.find(path =>
      path.hasOnlySameElement(_ ~= _) && (RegularPgon.edgesNumberFrom(path.safeHead) match {
        case Success(edgesNumber) => edgesNumber - (path.size + 1) - edgesSize == 0
        case Failure(_)           => false
      }))

  private def isPolygonPath(end1: Int, end2: Int, edgesSize: Int): Boolean = {
    val (first, second) = endPointsOrderedIndexes(end1, end2)
    val angles          = self.asInstanceOf[Tiling].perimeterAngles
    val paths           = angles.circularPathsIndexesExcluded(first, second)
    if (paths.exists(_.isEmpty)) {
      val a = RegularPgon.angleFrom(edgesSize + 1)

      def checkRoomForAngle(angle: Double): Boolean = mod(angle, TAU) >> a

      checkRoomForAngle(angles(first)) && checkRoomForAngle(angles(second))
    } else
      findPathFitting(paths, edgesSize).isDefined
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

  protected val positiveNodesAlreadyChecked = true

  private def goToPostAdd(newPerimeterEdges: Seq[Side[Int]], oldPerimeterEdges: List[Side[Int]]): Result = {
    // new edges changed to perimeter true
    newPerimeterEdges.foreach(_.isPerimeter = Some(true))
    Result(PostCheck, positiveNodesAlreadyChecked, gapChecked = true, oldPerimeterEdges)
  }

  /** Adding one node can never result into a valid tessellation. */
  override def preAdd(node: N): PreCheckResult = {
    lazyInfo("Starting pre-add single node " + node.toString)

    val isContained = self contains node
    lazyDebug("> node contained: " + isContained)
    if (isContained) PreCheckResult(Complete)
    else PreCheckResult(Abort)
  }

  /**
    * Single edge must be attached to perimeter
    * There is a special case where the perimeter can be calculated
    */
  override def preAdd(edge: E[N]): Result = {
    lazyInfo("Starting pre-add single edge " + edge.toString)

    val (end1, end2) = edge.toList.map(_.asInstanceOf[Int]).onlyTwoElements((_, _))

    if (!attachedToPerimeter(end1, end2)) {
      lazyDebug("> endpoints of single edge " + end1 + "~" + end2 + " must be both on perimeter")
      Result(Abort)
    } else if (isPolygonPath(end1, end2, 1))
      goToPostAdd(Seq(edge.asInstanceOf[Side[Int]]), selfPathEdges(end1, end2).toList)
    else if (isSquareDiagonal(end1, end2))
      Result(PostCheck, positiveNodesAlreadyChecked)
    else
      Result(Abort)
  }

  /**
    * If edges form a single path, this must be attached to perimeter
    * In this case, there is a nested special case where the perimeter can be calculated
    * and nested inside another more special case where the gaps need not be checked
    */
  override def preAdd(elems: InParam[N, E]*): Result = {
    lazyInfo("Starting pre-add elements check")
    lazyDebug("elems: " + elems.toString)

    val nodes: Seq[Int] = elems.filter(_.isNode).map(_.asInstanceOf[Int])
    lazyTrace("> Nodes being added: " + nodes.toString)

    if (nodes.exists(_ <= 0)) {
      lazyDebug("> added non positive nodes = " + nodes.filter(_ <= 0))
      Result(Abort)
    } else {
      val edges: Seq[Side[Int]] = elems.filter(_.isEdge).map(_.asInstanceOf[Side[Int]])
      lazyTrace("> Edges being added: " + edges.toString)

      if (edges.exists(_.toList.exists(_ <= 0))) {
        lazyDebug("> added non positive edges = " + edges.filter(_.toList.exists(_ <= 0)))
        Result(Abort)
      } else if (nodes.diff(edges.flatMap(_.toList)).nonEmpty) {
        lazyDebug("> added 1-degree nodes = " + nodes.diff(edges.flatMap(_.toList)))
        Result(Abort)
      } else {
        pathEndPoints(edges.toSet) match {
          // if the new edges form a path with two endpoints
          case Success((end1, end2)) =>
            if (attachedToPerimeter(end1, end2) && isPolygonPath(end1, end2, edges.size))
              goToPostAdd(edges, selfPathEdges(end1, end2).toList)
            else
              Result(Abort)
          case _ => Result(PostCheck, positiveNodesAlreadyChecked)
        }
      }
    }
  }

  private def setEdges(newGraph: Graph[N, E],
                       preCheck: PreCheckResult,
                       isPerimeter: Option[Boolean] = Some(true)): Unit =
    preCheck match {
      case r: Result if r.perimeterEdges.nonEmpty =>
        newGraph.edges
          .intersect(r.perimeterEdges map (old => newGraph get old.asInstanceOf[E[N]]))
          .foreach(_.asInstanceOf[Tiling#EdgeT].isPerimeter = isPerimeter)
      case _ =>
    }

  private def hasNoGaps(newGraph: Graph[N, E], preCheck: PreCheckResult): Boolean =
    preCheck match {
      case r: Result if r.gapChecked => true
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
      case r: Result => r.action + " " + r.gapChecked + " " + r.perimeterEdges
      case _         => "no Result"
    }))

    def positiveCheck: Boolean =
      preCheck match {
        case r: Result if r.action == positiveNodesAlreadyChecked => true
        case _ =>
          lazyDebug("> checking positive values")
          newGraph.hasPositiveValues
      }

    def isConnected: Boolean = {
      setEdges(newGraph, preCheck, Some(false))
      preCheck match {
        case r: Result if r.perimeterEdges.nonEmpty => true
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

    val isValid = positiveCheck && newGraph.hasRegularNodes && isConnected && hasPerimeter && polygonOk && hasNoGaps(newGraph,
                                                                                                       preCheck)
    if (!isValid) setEdges(newGraph, preCheck, isPerimeter = Some(true))
    isValid
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

  private val perimeterToBeRecalculated = true

  def checkNewPerimeter(end1: Int, end2: Int): Result =
    Result(PostCheck, gapChecked = true, perimeterEdges = selfPathEdges(end1, end2, onPerimeter = false).toList)

  /**
    * Ok if the node is at the center of an hexagon by 6 triangles
    * or at the center of a valid perimeter p-gon
    * Not ok if otherwise not on the perimeter
    * or if on the perimeter but leaving open perimeter edges
    * In all other cases needs a post check for the perimeter polygon
    */
  override def preSubtract(node: self.NodeT, forced: Boolean): Result = {
    lazyInfo("Starting pre-subtract node " + node.toString)
    lazyDebug("forced: " + forced)
    lazyDebug("self: " + self)

    if (node.degree == 6) {
      lazyDebug("> degree 6")
      Result(Complete)
    } else {
      lazyDebug("> degree < 6")
      val periNodes = self.asInstanceOf[Tiling].perimeterOrderedNodes.init
      node.toOuter.asInstanceOf[Int] match {
        case n if periNodes.contains(n) =>
          if (node.neighbors.exists(_.degree == 2)) {
            lazyDebug("> non removable perimeter node 1")
            Result(Abort)
          } else {
            // find existing non perimeter edges and transform to perimeter
            lazyDebug("> found existing edges")
            val (end1, end2) = periNodes.circularNeighborsOf(n).safeGet()
            checkNewPerimeter(end1, end2)
          }
        case _ =>
          if (RegPgon.edgesNumberToPgon.get(periNodes.length).isDefined &&
              periNodes.diff(self.nodes.filterNot(_ == node).toList.map(_.toOuter.asInstanceOf[Int])).isEmpty)
            Result(Complete)
          else {
            lazyDebug("> non removable perimeter node 2")
            Result(Abort)
          }
      }
    }
  }

  /**
    * Not ok if not connecting two perimeter nodes
    * Also not ok if perimeter leaving open other perimeter edges
    * In all other cases needs a post check for the perimeter polygon
    */
  override def preSubtract(edge: self.EdgeT, simple: Boolean): Result = {
    lazyInfo("Starting pre-subtract edge " + edge.toString)
    lazyDebug("simple " + simple)

    val (end1, end2) = edge.toList.map(_.toOuter.asInstanceOf[Int]).onlyTwoElements((_, _))

    if (!attachedToPerimeter(end1, end2)) {
      lazyDebug("> endpoints of single edge " + end1 + "~" + end2 + " must be both on perimeter")
      Result(Abort)
    } else {
      val periEdges = self.asInstanceOf[Tiling].perimeterOrderedEdges.init
      if (periEdges.contains(edge.toOuter.asInstanceOf[Side[Int]])) {
        if (edge.toList.exists(_.degree == 2)) {
          lazyDebug("> perimeter edge " + edge.toString + " has node of degree 2")
          Result(Abort)
        } else
          checkNewPerimeter(end1, end2)
      } else
        Result(PostCheck, gapChecked = true)
    }
  }

  /**
    * If nodes form a single perimeter path
    */
  override def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean): Result = {
    lazyInfo("Starting pre-subtract nodes and edges")
    lazyDebug("nodes: " + nodes.toString)
    lazyDebug("edges: " + edges.toString)
    lazyDebug("simple: " + simple)
    lazyTrace("self: " + self)

    nodes match {
      case all if all.size == self.nodes.length => Result(Complete)
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
              Result(Abort)
            }
          case _ => Result(PostCheck, perimeterToBeRecalculated)
        }
      case _ => Result(PostCheck, perimeterToBeRecalculated)
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
      case r: Result => r.action + " " + r.gapChecked + r.perimeterEdges
      case _         => ""
    }))

    lazyDebug("> checking nodes' degree")

    def isConnected: Boolean =
      preCheck match {
        case r: Result if r.gapChecked => true
        case _ =>
          lazyDebug("> checking connected")
          newGraph.isConnected
      }

    def hasPerimeter: Boolean = {
      setEdges(newGraph, preCheck)
      preCheck match {
        case r: Result if r.action == perimeterToBeRecalculated || !newGraph.asInstanceOf[Tiling].hasPerimeterSet =>
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

object ByRegularPgons extends ConstraintCompanion[ByRegularPgons] {
  def apply[N, E[X] <: EdgeLikeIn[X]](self: Graph[N, E]): ByRegularPgons[N, E] = new ByRegularPgons[N, E](self)
}
