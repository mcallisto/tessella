package vision.id.tessella

import scala.util.{Success, Try}

import scalax.collection.GraphPredef.{EdgeLikeIn, InParam}
import scalax.collection.constrained.PreCheckFollowUp._
import scalax.collection.constrained.{ConstraintCompanion, Graph, PreCheckResult, PreCheckResultCompanion}

import vision.id.tessella.Tessella.Tiling
import vision.id.tessella.Others.Mono
import vision.id.tessella.Tau.TAU

class Monogonal[N, E[X] <: EdgeLikeIn[X]](override val self: Graph[N, E])
    extends ByRegularPgons(self: Graph[N, E])
    with ConstraintUtils {

  protected class MonoResult(followUp: PreCheckFollowUp,
                             action: Boolean = false,
                             gapChecked: Boolean = false,
                             perimeterEdges: List[Side[Int]] = Nil,
                             val monoChecked: Boolean = false)
      extends Result(followUp, action, gapChecked, perimeterEdges)

  protected object MonoResult extends PreCheckResultCompanion {
    def apply(followUp: PreCheckFollowUp) =
      new MonoResult(followUp, action = false, gapChecked = false, perimeterEdges = Nil, monoChecked = false)
    def apply(followUp: PreCheckFollowUp,
              action: Boolean = false,
              gapChecked: Boolean = false,
              perimeterEdges: List[Side[Int]] = Nil,
              monoChecked: Boolean = false) =
      new MonoResult(followUp, action, gapChecked, perimeterEdges, monoChecked)
  }

  private def isPolygonPath(end1: Int, end2: Int, edgesSize: Int, full: Full): Boolean = {

    val (first, second) = endPointsOrderedIndexes(end1, end2)
    val angles          = self.asInstanceOf[Tiling].perimeterAngles
    val vertices        = self.asInstanceOf[Tiling].vertexes

    def areEndPointVerticesContained(pgon: RegPgon, pathIndexes: List[Int]): Boolean = {

      def addDirectionally(v: Vertex, direction: Boolean): Try[Vertex] =
        if (direction) v.prepend(pgon) else v.append(pgon)

      val nodes                 = self.asInstanceOf[Tiling].perimeterOrderedNodes.init
      val (orderedNeighbors, _) = self.asInstanceOf[Tiling].perimeterHoods.unzip
      val pathNodes             = pathIndexes.map(nodes(_))

      def findDir(index: Int): Boolean = {
        val startNode = orderedNeighbors(index).safeHead
        pathNodes match {
          case Nil => nodes(index) != startNode
          case ns  => ns.contains(startNode)
        }
      }

      sequence(List(first, second).map(i => addDirectionally(vertices(i), findDir(i)))) match {
        case Success(grownVertices) => grownVertices.forall(_.isContainedIn(full))
        case _                      => false
      }
    }

    val (pathsAngles, pathsIndexes) = angles.zipWithIndex.circularPathsIndexesExcluded(first, second).map(_.unzip).unzip
    if (pathsAngles.exists(_.isEmpty)) {
      val pgon = RegPgon.ofEdges(edgesSize + 1).safeGet

      def checkRoomForAngle(angle: Double): Boolean = mod(angle, TAU) >> pgon.alpha

      checkRoomForAngle(angles(first)) && checkRoomForAngle(angles(second)) && areEndPointVerticesContained(pgon, Nil)
    } else
      findPathFitting(pathsAngles, edgesSize) match {
        case Some(pathAngles) =>
          val pgon  = RegPgon.fromAlpha(pathAngles.safeHead).safeGet
          val index = pathsAngles.indexOf(pathAngles)
          pathsIndexes(index)
            .map(vertices(_))
            .forall(_.append(pgon) match {
              case Success(vertex) if vertex.isFull => new Full(vertex.pgons).isEquivalentTo(full)
              case _                                => false
            }) && areEndPointVerticesContained(pgon, pathsIndexes(index))
        case None => false
      }
  }

  private def goToPostAdd(newPerimeterEdges: Seq[Side[Int]], oldPerimeterEdges: List[Side[Int]]): MonoResult = {
    lazyInfo("MONO Ordering to skip check")
    // new edges changed to perimeter true
    newPerimeterEdges.foreach(_.isPerimeter = Some(true))
    MonoResult(PostCheck, positiveNodesAlreadyChecked, gapChecked = true, oldPerimeterEdges, monoChecked = true)
  }

  override def preAdd(edge: E[N]): Result = {
    lazyInfo("MONO Starting pre-add single edge " + edge.toString)

    self.asInstanceOf[Mono].findFull match {
      case Some(full) =>
        lazyDebug("MONO full: " + full)
        val (end1, end2) = edge.toList.map(_.asInstanceOf[Int]).onlyTwoElements((_, _))
        if (attachedToPerimeter(end1, end2) && isPolygonPath(end1, end2, edgesSize = 1, full))
          goToPostAdd(Seq(edge.asInstanceOf[Side[Int]]), selfPathEdges(end1, end2).toList)
        else
          super.preAdd(edge)
      case None => super.preAdd(edge)
    }
  }

  override def preAdd(elems: InParam[N, E]*): Result = {
    lazyInfo("MONO Starting pre-add elements check")
    lazyDebug("elems: " + elems.toString)

    self.asInstanceOf[Mono].findFull match {
      case Some(full) =>
        lazyDebug("MONO full: " + full)
        val nodes: Seq[Int]       = elems.filter(_.isNode).map(_.asInstanceOf[Int])
        val edges: Seq[Side[Int]] = elems.filter(_.isEdge).map(_.asInstanceOf[Side[Int]])

        if (nodes.exists(_ <= 0)
            || edges.exists(_.toList.exists(_ <= 0))
            || nodes.diff(edges.flatMap(_.toList)).nonEmpty)
          Result(Abort)
        else
          pathEndPoints(edges.toSet) match {
            // if the new edges form a path with two endpoints
            case Success((end1, end2)) =>
              if (!attachedToPerimeter(end1, end2)) {
                lazyDebug("> endpoints of edges path must be both on perimeter " + edges.toString)
                Result(Abort)
              } else {
                // find old perimeter edges to be passed to postCheck
                val oldEdges = selfPathEdges(end1, end2)
                lazyTrace(oldEdges.toString)

                if (/*oldEdges.size == 1 || */ isPolygonPath(end1, end2, edges.size, full))
                  goToPostAdd(edges, oldEdges.toList)
                else Result(Abort)
              }
            case _ => Result(PostCheck, positiveNodesAlreadyChecked)
          }
      case None => super.preAdd(elems: _*)
    }
  }

  override def postAdd(newGraph: Graph[N, E],
                       passedNodes: Traversable[N],
                       passedEdges: Traversable[E[N]],
                       preCheck: PreCheckResult): Boolean = {
    def isMono: Boolean = preCheck match {
      case r: MonoResult if r.monoChecked => true
      case _                              => newGraph.asInstanceOf[Mono].isMonogonal
    }

    super.postAdd(newGraph, passedNodes, passedEdges, preCheck) && isMono
  }
}

object Monogonal extends ConstraintCompanion[Monogonal] {
  def apply[N, E[X] <: EdgeLikeIn[X]](self: Graph[N, E]): Monogonal[N, E] = new Monogonal[N, E](self)
}
