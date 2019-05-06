package vision.id.tessella

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge

import vision.id.tessella.Cartesian2D._
import vision.id.tessella.Tau.TAU
import vision.id.tessella.Tessella.Tiling

trait ConstraintUtils extends TilingUtils with Grid {

  final implicit class CTiling(tiling: Tiling) {

    private implicit class CNode(node: tiling.NodeT) {

      def isPerimeter: Boolean = tiling.perimeterOrderedNodes.contains(node.toOuter)

      def neighs: (List[Int], List[List[Int]]) = tiling.outerNodeHood(node.toOuter, tiling.perimeterOrderedNodes)

      def isOneNodeFromPeri: Boolean = node.neighbors.exists(_.isPerimeter)

      def isTwoOrLessNodesFromPeri: Boolean =
        (node.neighbors.foldLeft(Set(): Set[tiling.NodeT])(_ ++ _.neighbors) - node).exists(_.isPerimeter)

    }

    def toGonals(nm: NodesMap, withPerimeter: Boolean = false): List[List[Point2D]] =
      mapGonals(withPerimeter).map({ case (_, nodes) => nodes.map(nm.m(_)) }).toList

    def toGrid(nm: NodesMap): Grid = {
      val l = tiling.edges.toList.map(
        _.nodes
          .map(_.toOuter)
          .toList
          .onlyTwoElements((f, s) => OrderedUnitSegment2D.fromPoint2Ds(nm.m(f), nm.m(s))))
      SortedSet(l: _*)
    }

    def toUniforms(nm: NodesMap, buffer: Int = 2): List[List[List[Point2D]]] =
      mapUniforms(buffer).map({ case (_, listNodes) => listNodes.map(_.map(nm.m(_))) }).toList

    // ----------------- gonality -------------------

    private def pathToFull(path: List[List[Int]]): Full = Full.p(path.map(_.size + 2)).minor

    private def fullNodesMap: Map[tiling.NodeT, Full] =
      tiling.nodes
        .filterNot(_.isPerimeter)
        .map(node => node.neighs match { case (_, path) => node -> pathToFull(path) })
        .toMap

    def distinctFullVertices: List[Full] = fullNodesMap.values.toList.distinct

    /**
      * @return map of different type of vertices and nodes where they are found
      */
    def mapGonals(withPerimeter: Boolean = false): Map[Full, List[Int]] = {
      val m = fullNodesMap.groupBy({ case (_, vertex) => vertex }).mapValues(_.keys.toList.map(_.toOuter))
      if (!withPerimeter) m
      else
        m.keys.foldLeft(m)((mm, full) => {
          val others = m.keys.toList.diff(List(full))
          val nodes = for {
            i <- tiling.vertexes.indices
            if tiling.vertexes(i).isContainedIn(full) && others.forall(!tiling.vertexes(i).isContainedIn(_))
          } yield tiling.perimeterOrderedNodes(i)
          mm.updated(full, mm(full) ++ nodes.toList)
        })
    }

    /**
      * @return number of different type of vertices
      */
    def gonality: Int = mapGonals().size

    def isMonogonal: Boolean = {

      def checkIfSameVertex: Boolean = distinctFullVertices match {
        case full :: Nil => tiling.vertexes.forall(_.isContainedIn(full))
        case _           => false
      }

      @tailrec
      def combinedVertices(l: List[Vertex], acc: List[Vertex]): List[Vertex] = l match {
        case Nil    => acc
        case h :: t => combinedVertices(t, acc.flatMap(_.merge(h)))
      }

      tiling.nodes.toList.diff(tiling.perimeterOrderedNodes.map(tiling.get)) match {
        case node :: nodes => nodes.forall(_.degree == node.degree) && checkIfSameVertex
        case Nil =>
          tiling.vertexes.distinct match {
            case h :: t => combinedVertices(t, List(h)).nonEmpty
            case Nil    => throw new Error
          }
      }

    }

    def findFull: Option[Full] =
      tiling.nodes
        .find(!tiling.perimeterOrderedNodes.map(tiling.get).contains(_))
        .map(_.neighs match { case (_, path) => pathToFull(path) })

    // ----------------- equality -------------------

    /**
      * - find all rotareflections with same perimeter p-gon
      * - annotate rotation steps and reflection
      * - for each candidate transform into grid
      *   - find starting point and align it with reference
      *   - if mapped inverted, flipH on starting point
      *   - rotate centered on starting point to align next point
      *   - if all segments align, is equal
      * - no success, is not equal
      *
      * @param that other tessellation
      * @return
      */
    def isEquivalentTo(that: Tiling): Boolean = {
      val thisSize = tiling.polygon.n
      if (thisSize != that.polygon.n || tiling.graphSize != that.graphSize) false
      else {
        val keys: IndexedSeq[(Int, Boolean)] = (0 until thisSize).flatMap(i => List((i, false), (i, true)))
        val perimeterEquivalents: IndexedSeq[(Int, Boolean)] = keys
          .zip(that.polygon.points.rotaReflections)
          .filter({ case (_, candidate) => tiling.polygon.points == candidate })
          .map({ case ((rotations, isReflected), _) => (rotations, isReflected) })
        val (thisNm, thatNm)         = (tiling.toNodesMap, that.toNodesMap)
        val (thisGrid, thatGrid)     = (toGrid(thisNm), that.toGrid(thatNm))
        val (thisNode1, thisNode2)   = tiling.perimeterOrderedNodes.take(2).onlyTwoElements((_, _))
        val (thisPoint1, thisPoint2) = (thisNm.m(thisNode1), thisNm.m(thisNode2))
        val thisRotation             = thisPoint2.angleFrom(thisPoint1)

        perimeterEquivalents.exists({
          case (rotations, isReflected) =>
            // index of shifted first
            val index = (if (isReflected) thisSize - 1 else 0) - rotations
            val (kNode1, kNode2) = (
              that.perimeterOrderedNodes(mod(index, thisSize)),
              that.perimeterOrderedNodes(mod(index + (if (isReflected) -1 else 1), thisSize))
            )
            // check if map is drawn inverted in respect of this
            val mappedInverted     = thatNm.rotationDir(kNode1, kNode2) != thisNm.rotationDir(thisNode1, thisNode2)
            val (kPoint1, kPoint2) = (thatNm.m(kNode1), thatNm.m(kNode2))
            val translation        = thisPoint1.diff(kPoint1)
            val kRotation          = kPoint2.sum(translation).angleFrom(thisPoint1)
            val rotation           = thisRotation - kRotation - (if (mappedInverted) TAU / 2 else 0)
            val kGrid = thatGrid
              .map(_.sum(translation))
              .map(s => if (mappedInverted) s.flipH(thisPoint1.x) else s)
              .map(_.rotate(rotation, thisPoint1))
            kGrid == thisGrid
        })
      }
    }

    // ----------------- uniformity -------------------

    private type nodesClasses = List[List[Int]]

    /**
      * @param symmetries map showing if couple of nodes is symmetric or not
      * @return
      */
    private def findClasses(symmetries: Map[(Int, Int), Boolean]): nodesClasses = {
      // only pairs deemed not symmetric
      val asymmetricPairs = symmetries.filterNot({ case (_, symmetric) => symmetric })
      //println(asymmetricPairs)
      // nodes from pairs
      val asymmetricNodes = asymmetricPairs.flatMap({ case ((f, s), _) => List(f, s) }).toList.distinct
      // only symmetric but with both nodes not symmetric to others
      val edges: Iterable[UnDiEdge[Int]] =
        symmetries
          .filter({
            case ((f, s), symmetric) => symmetric && asymmetricNodes.contains(f) && asymmetricNodes.contains(s)
          })
          .map({ case ((f, s), _) => UnDiEdge(f, s) })
      // transform them into a graph
      // is the complete graph of the asymmetric nodes without the asymmetric pairs
      //println(edges)
      val gg: Graph[Int, UnDiEdge] = Graph.from(edges = edges)
      // graph components are the different classes
      gg.componentTraverser().toList.map(_.nodes.map(_.toOuter).toList)
    }

    /** @todo remember this works only for tessellations with 3, 4, 6, 8, 12 p-gons
      */
    private def symmetries(vertices: List[Full]): Int =
      vertices.flatMap(_.edgesNumbers).distinct.sorted match {
        case List(6)    => 3
        case List(3)    => 6
        case List(4)    => 4
        case List(4, 8) => 4
        case List(3, 6) => 6
        case _          => 12
      }

    private def filterNodes(nodes: List[Int], buffer: Int): List[Int] = buffer match {
      case 0 => nodes
      case 1 => nodes.filterNot(tiling.get(_).isOneNodeFromPeri)
      case _ => nodes.filterNot(tiling.get(_).isTwoOrLessNodesFromPeri)
    }

    private def areSymmetric(point1: Point2D,
                             point2: Point2D,
                             filteredGrids: List[Grid],
                             periPolygon: Polygon): Boolean = {
      val symm               = filteredGrids.size / 2
      val originalGrid: Grid = filteredGrids.safeHead
      // for each rotation move the grids to the right alignment of point2 with point1
      val grids: IndexedSeq[Grid] = (0 until symm).flatMap(i => {
        val pointRotated = point2.rotate(i * TAU / symm)
        List(
          filteredGrids(i).map(_.sum(point1.diff(pointRotated))), // rotations
          filteredGrids(i + symm).map(_.sum(point1.diff(pointRotated.mult(-1, 1)))) // reflections
        )
      })
      // filter out segments not included in the polygon
      val included: IndexedSeq[Grid] = grids.map(_.filter(periPolygon.includesUnit))
      included.exists(_.isSubsetOf(originalGrid))
    }

    /**
      * @return map of different type of vertices and nodes in different classes of symmetry
      */
    def mapUniforms(buffer: Int = 2): Map[Full, nodesClasses] = {
      val nm                                               = tiling.toNodesMap
      val periPolygon                                      = tiling.toPerimeterPolygon(nm)
      val (vertices, nClasses): (List[Full], nodesClasses) = mapGonals(withPerimeter = true).toList.unzip
      // only with the needed rotations
      val filteredGrids = tiling.toGrid(nm).rotatedGrids(symmetries = symmetries(vertices))

      //    logger.debug("\nNumber of symmetry axes searched: " + symm)
      val classes: List[nodesClasses] = vertices.indices.foldLeft(List(): List[nodesClasses])((l, index) => {
        val filteredNodes: List[Int] = filterNodes(nClasses(index), buffer)
        //        logger.debug("\nfilteredNodes: " + filteredNodes)
        val symmetricPairs =
          filteredNodes
            .combinations(2)
            .map(_.onlyTwoElements((f, s) => (f, s) -> areSymmetric(nm.m(f), nm.m(s), filteredGrids, periPolygon)))
            .toMap
        //        logger.debug("\nsymmetries: " + symmetricPairs)
        val nodeClass =
          if (symmetricPairs.values.forall(_ == true)) List(filteredNodes)
          else {
            // @todo needs to be checked and clarified
            val foundClasses: nodesClasses = findClasses(symmetricPairs)
            val singles: List[Int]         = filteredNodes.diff(foundClasses.flatten)
            foundClasses ++ singles.map(List(_))
          }
        nodeClass +: l
      })
      vertices.zip(classes.reverse).toMap
    }

    /**
      * @return map of different type of vertices and nodes in different classes of symmetry
      */
    def isUniformAlt(buffer: Int = 2): Boolean = {
      val nm                                               = tiling.toNodesMap
      val periPolygon                                      = tiling.toPerimeterPolygon(nm)
      val (vertices, nClasses): (List[Full], nodesClasses) = mapGonals(withPerimeter = true).toList.unzip
      // only with the needed rotations
      val filteredGrids = tiling.toGrid(nm).rotatedGrids(symmetries = symmetries(vertices))

      vertices match {
        case _ :: Nil =>
          filterNodes(nClasses.safeHead, buffer)
            .combinations(2)
            .forall(_.onlyTwoElements((f, s) => areSymmetric(nm.m(f), nm.m(s), filteredGrids, periPolygon)))
        case _ => false
      }
    }

    /**
      * @return number of different classes of symmetry contained
      */
    def uniformity(buffer: Int = 2): Int = mapUniforms(buffer).values.map(_.size).sum

    def isUniform: Boolean = uniformity() == 1

  }

}
