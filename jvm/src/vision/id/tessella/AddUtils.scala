package vision.id.tessella

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import scalax.collection.GraphEdge.UnDiEdge

import vision.id.tessella.Tessella.Tiling
import vision.id.tessella.Polar.RegularPgon
import vision.id.tessella.Tau.TAU

trait AddUtils extends TilingUtils with MathUtils {

  final implicit class ATiling(tiling: Tiling) {

    private implicit class ANode(node: tiling.NodeT) {

      private def orderedIndex: Int = tiling.perimeterOrderedNodes.indexOf(node.toOuter)

      /**
        * get nodes ordered from a given start
        *
        * @param direction true is following orderedNodes, false other way
        * @throws NoSuchElementException if node is not found
        * @return
        */
      def startNodes(direction: Boolean = true): List[Int] = orderedIndex match {
        case i => tiling.perimeterOrderedNodes.init.rotateWithDirection(-i, direction)
      }

      def startVertexes(dir: Boolean = true): List[Vertex] = tiling.vertexes.rotateWithDirection(-orderedIndex, dir)

//      def orderedVertex: Vertex = tiling.vertexes(orderedIndex)

      /**
        * get the node where to attach a p-gon, it can move because of adjacent p-gons
        *
        * @param a   internal angle of the p-gon to be attached
        * @param dir direction along the perimeter, true is following perimeterOrderedNodes, false other way
        * @return id and edges to be subtracted from addition
        */
      def getAttachNode(a: Double, dir: Boolean): Try[(Int, List[tiling.EdgeT])] = {
        val ns = node.startNodes(dir)

        @tailrec
        def loop(vertices: List[Vertex], builder: tiling.PathBuilder, index: Int): Try[(Int, List[tiling.EdgeT])] =
          vertices match {
            case Nil => throw new IllegalArgumentException("no more sides")
            case h :: t =>
              h.alpha match {
                case adjacent if a + adjacent ~= TAU =>
                  loop(t, builder += tiling.get(ns(index + 1)), index + 1) // go to next
                case over if a + over > TAU =>
                  throw new IllegalArgumentException("angle more than full, dir: " + dir)
                case _ => Try(ns(index), builder.result().edges.toList)
              }
          }

        loop(node.startVertexes(dir), tiling.newPathBuilder(node), 0)

      }

    }

    private implicit class AEdge(edge: tiling.EdgeT) {

      /**
        * @throws NoSuchElementException if edge is not found
        * @return (first, second) endpoint nodes of the edge ordered
        */
      def orderedEndPoints: (tiling.NodeT, tiling.NodeT) = tiling.perimeterOrderedEdges.indexOf(edge.toOuter) match {
        case -1 => throw new IllegalArgumentException("cannot find edge")
        case i  => (tiling get tiling.perimeterOrderedNodes(i), tiling get tiling.perimeterOrderedNodes(i + 1))
      }

      /**
        * get new ids for p-gons to be added
        *
        * @param n          number of new ids needed
        * @param emptiesMax max id and unused ones in between
        * @return
        */
      private def getFreeIds(n: Int, emptiesMax: (List[Int], Int)): List[Int] = n match {
        case e if e < 0 => throw new Error
        case 0          => List()
        case _ =>
          val (es, max) = emptiesMax
          val s         = es.size
          if (s >= n) es.take(n)
          else es ++ ((max + 1) to (max + n - s)).toList
      }

      /**
        * get new perimeter edges to be added to the tiling if a p-gon is added
        * (and the old ones no longer in the perimeter)
        *
        * @param edgesNumber number of edges of added p-gon
        * @return
        */
      def additionalEdges(edgesNumber: Int): Try[(List[Side[Int]], List[Side[Int]])] = {
        val (f, s) = edge.orderedEndPoints
        val angle  = RegularPgon.angleFrom(edgesNumber)
        f.getAttachNode(angle, dir = false)
          .flatMap({
            case (back_node, backOldPerimeterEdges) =>
              s.getAttachNode(angle, dir = true)
                .flatMap({
                  case (forward_node, forwardOldPerimeterEdges) =>
                    val ids: List[Int] =
                      getFreeIds(edgesNumber - 2 - backOldPerimeterEdges.size - forwardOldPerimeterEdges.size,
                                 tiling.toG.emptiesMax)
                    val newPerimeterEdges: List[Side[Int]] = ids match {
                      case Nil => List(Side(back_node, forward_node))
                      case _ =>
                        ids.indices.tail.toList.map(i => Side(ids(i - 1), ids(i))) ++
                          List(Side(back_node, ids.safeHead), Side(ids.safeLast, forward_node))
                    }
                    Success((newPerimeterEdges, (backOldPerimeterEdges ++ forwardOldPerimeterEdges).map(_.toOuter)))
                })
          })
      }

      /**
        * try to add a p-gon to the given edge
        *
        * @param edgesNumber sides of the reg p-gon to be added
        * @return
        */
      def addPgonOfEdges(edgesNumber: Int): Try[Tiling] =
        edge.additionalEdges(edgesNumber).flatMap({ case (newPerimeterEdges, _) => Try(tiling ++ newPerimeterEdges) })

      def addPgonOfEdges2(edgesNumber: Int): Try[Unit] =
        edge.additionalEdges(edgesNumber).flatMap({ case (newPerimeterEdges, _) => Try(tiling ++= newPerimeterEdges) })

      def minFirstEndpoint: (tiling.NodeT, tiling.NodeT) = edge.nodes.toList match {
        case f :: s :: Nil if f < s => (f, s)
        case f :: s :: Nil          => (s, f)
        case _                      => throw new Error
      }

    }

    def addToEdgePgon(edge: Side[Int], edgesNumber: Int): Try[Tiling] = (tiling get edge).addPgonOfEdges(edgesNumber)

    def addToEdgePgon2(edge: Side[Int], edgesNumber: Int): Try[Unit] = (tiling get edge).addPgonOfEdges2(edgesNumber)

    type EdgeNumbers = Int

    /**
      * list of (index of pattern, number of edges of pgon)
      */
    type Attr = List[(Int, EdgeNumbers)]

    /**
      * couple of attributes and angle for one direction and the other
      */
    type Couples = ((Attr, Attr), (Double, Double))

    /**
      * (number of edges of pgon, list of indexes of pattern)
      */
    type Attr2 = (EdgeNumbers, List[Int])

    /**
      * couple the edge attributes of the same edge
      *
      * @param edgeAttrs attributes and angles of the perimeter edges, it assumes each edge is listed exactly twice
      * @tparam E edge type
      * @return map each edge to couples of attributes and angles
      */
    def coupleEdges[E](edgeAttrs: List[(E, Attr, Double)]): Map[E, Couples] =
      edgeAttrs
        .groupBy({ case (edge, _, _) => edge }) // grouped by edge
        .mapValues(_.map({
          case (_, indexesNumbers, angle) => (indexesNumbers, angle)
        }).unzip) // discard edge in values
        .mapValues({
          case (indexesNumbers, angles) => ((indexesNumbers.safeHead, indexesNumbers(1)), (angles.safeHead, angles(1)))
        }) // create couples

    /**
      * intersect p-gons found in the two directions
      *
      * @param indexesNumbers1 list of indexes and numbers of edges for the edge seen from one endpoint
      * @param indexesNumbers2 list of indexes and numbers of edges for the same edge seen from other endpoint
      * @return
      */
    private def intersectsSides(indexesNumbers1: Attr, indexesNumbers2: Attr): List[Int] = indexesNumbers1.unzip match {
      case (_, sides1) =>
        sides1.intersect(indexesNumbers2.unzip match {
          case (_, sides2) => sides2
        })
    }

    /**
      * if no intersection the edge cannot be expanded
      *
      * @tparam E edge type
      * @return
      */
    def checkExpansion[E]: ((E, Couples)) => Boolean = {
      case (_, ((indexesNumbers1, indexesNumbers2), _)) => intersectsSides(indexesNumbers1, indexesNumbers2).nonEmpty
    }

    type Group = (Map[Int, List[Int]], Double)

    /**
      * map patterns for each side
      *
      * @param coupled map of edges with associated attributes and angles
      * @tparam E edge type
      * @return
      */
    def groupSides[E](coupled: Map[E, Couples]): Map[E, Group] =
      coupled
        .filter(checkExpansion)
        .mapValues({
          case ((indexesNumbers1, indexesNumbers2), angles) =>
            val intersected: List[Int] = intersectsSides(indexesNumbers1, indexesNumbers2).distinct
            ((
               indexesNumbers1.filter({ case (_, edgeNumbers) => intersected.contains(edgeNumbers) }),
               indexesNumbers2.filter({ case (_, edgeNumbers) => intersected.contains(edgeNumbers) })
             ),
             angles)
        })
        .mapValues({
          case ((indexesNumbers1, indexesNumbers2), (angle1, angle2)) =>
            (
              (indexesNumbers1 ++ indexesNumbers2)
                .groupBy({ case (_, edgeNumbers: Int) => edgeNumbers })
                .mapValues(_.unzip match { case (indexes, _) => indexes })
                //        .filter({ case (_, indexes) => indexes.lengthCompare(2) >= 0 && indexes != indexes.distinct }),
                .filter({ case (_, indexes) => indexes.lengthCompare(2) >= 0 }),
              Math.min(angle1, angle2) // get minimum angle
            )
        })

    /**
      * split in separate blocks giving priority to edges with less solutions
      *
      * @param grouped map of edges
      * @param mandatory if true only block with one solution
      * @tparam E edge type
      * @return
      */
    def splitBySolutionsSize[E](grouped: Map[E, Group], mandatory: Boolean = false): List[Map[E, Group]] =
      grouped
        .filter({ case (_, (m, _)) => if (mandatory) m.size == 1 else true })
        .groupBy({ case (_, (m, _)) => m.size })
        .toList
        .sortBy({ case (size, _) => size })
        .map({ case (_, m) => m })

    /**
      * reunite blocks, after sorting each with additional criteria
      *
      * @param splitted separated block by solution size
      * @param fsort sorting criteria
      * @tparam E edge type
      */
    def joinSorted[E](splitted: List[Map[E, Group]],
                      fsort: ((E, Attr2, Double)) => (Double, Int, Int, Int)): List[(E, Attr2, Double)] =
      splitted.flatMap(
        _.flatMap({ case (edge, (m, angle)) => m.toList.map((edge, _, angle)) }).toList
          .sortBy(fsort))

    /**
      * third element is exterior angle at one endpoint
      */
    private type EdgeAttr = (tiling.EdgeT, Attr, Double)

    /**
      * third element is minimum exterior angle at endpoints
      */
    private type EdgeAttr2 = (tiling.EdgeT, Attr2, Double)

    /**
      * find for each vertex the p-gons that could be added to continue the given pattern
      *
      * @param patterns full vertices
      * @param i        index of the ordered perimeter nodes
      * @return
      */
    private def checkPatterns(patterns: List[Full], i: Int): List[EdgeAttr] = {
      val v     = tiling.vertexes(i)
      val e1    = tiling get (if (i == 0) tiling.perimeterOrderedEdges.safeLast else tiling.perimeterOrderedEdges(i - 1))
      val e2    = tiling get tiling.perimeterOrderedEdges(i)
      val angle = v.beta
      List(
        (e1, patterns.indices.flatMap(j => patterns(j).findNexts(v).map((j, _))).toList, angle),
        (e2, patterns.indices.flatMap(j => patterns(j).findNexts(v, reversed = true).map((j, _))).toList, angle)
      )
    }

    /**
      *
      * @param patterns distinct full vertices
      * @param infinite if true, end search if just 1 edge cannot be expanded
      * @return
      */
    private def nextCandidates(patterns: List[Full],
                               infinite: Boolean = false,
                               mandatory: Boolean = false): List[EdgeAttr2] = {

      val attributes: List[EdgeAttr] =
        tiling.perimeterOrderedNodes.init.indices.flatMap(i => checkPatterns(patterns, i)).toList
      //    logger.debug("\nattributes: " + attributes.mkString("\n"))

      // associate couples of edge attributes
      val coupled: Map[tiling.EdgeT, Couples] = coupleEdges(attributes)
      //    logger.debug("\ncoupled: " + coupled.mkString("\n"))

      val isExpandable = coupled.forall(checkExpansion)
      //    logger.debug("\n isExpandable: " + isExpandable)

      if (infinite && !isExpandable) Nil
      else {

        // split in separate blocks giving priority to edges with less solutions
        val splitBySize: List[Map[tiling.EdgeT, Group]] =
          splitBySolutionsSize(groupSides(coupled), mandatory)
        //      logger.debug("\nsortedBySolutionsSize: " + sortedBySolutionsSize.mkString("\n"))

        // possible sorting criteria:
        // - indexes
        //   - size: less?
        //   - uniformity: solutions from same pattern preferrable?
        // - angle: minor
        // - side: priority to smaller or bigger p-gons?
        // - edge: with the smallest endpoint could help gravitate around the centre
        def fsort: EdgeAttr2 => (Double, Int, Int, Int) = {
          case (edge, (edgeNumbers, indexes), angle) =>
            val (nmin, nmax) = edge.minFirstEndpoint
            patterns.size match {
              //  case 1 => (indexes.size.toDouble, (angle * 100).toInt, 0, 0)
              //  case 1 => (angle, Math.min(edge._1.toOuter, edge._2.toOuter), 0, 0)
              //  case 1 => (angle, nmin.toOuter, nmax.toOuter, 0)
              //  case 1 => (angle, indexes.size, nmin.toOuter, 0)
              case 1 => (angle, nmax.toOuter, 0, 0)
              case _ => (angle, nmin.toOuter, nmax.toOuter, edgeNumbers)
            }
        }

        // rejoin after sorting each block
        joinSorted(splitBySize, fsort)
      }
    }

    /**
      * already tested -and failed- possible addition
      */
    protected type Fail = (Side[Int], Int)

    // failed relevant to the current perimeter
    private def actualFails(failed: List[Fail]): List[(tiling.EdgeT, Int)] =
      failed.flatMap({
        case (edge, edgeNumbers) =>
          tiling.find(edge) match {
            case Some(e) => List((e, edgeNumbers))
            case None    => Nil
          }
      })

    def addPatterns(patterns: List[Full],
                     infinite: Boolean = false,
                     failed: List[Fail] = Nil,
                     mandatory: Boolean = false): Try[List[Fail]] = {

      val dpatterns = Full.distinct(patterns)

      val candidates: List[EdgeAttr2] = nextCandidates(dpatterns, infinite, mandatory)

      val actualFailed = actualFails(failed)

      val nexts: List[(tiling.EdgeT, Int)] = candidates
        .map({ case (edge, (edgeNumbers, _), _) => (edge, edgeNumbers) })
        .diff(actualFailed)

      for (i <- nexts.indices) {
        val (edge, edgeNumbers) = nexts(i)
        edge.additionalEdges(edgeNumbers) match {
          case Success((edges, _)) =>
            Try(tiling ++ edges) match {
              case Success(newt) =>
                def compatibleWithPatternsAtNode(nO: Int, patterns: List[Full]): Boolean =
                  patterns.exists(p => newt.vertexes(newt.perimeterOrderedNodes.indexOf(nO)).isContainedIn(p))

                pathEndPoints(edges.map(_.asInstanceOf[UnDiEdge[Int]]).toSet) match {
                  case Success((end1, end2)) =>
                    // check if the vertex formed at each endpoint is compatible with pattern
                    val isCompatible = List(end1, end2).forall(n => compatibleWithPatternsAtNode(n, dpatterns))
                    if (isCompatible) {
                      //                      if (!mandatory && dpatterns.lengthCompare(2) == 0 && newt.uniformity() != 2)
                      //                        logger.debug("\nfailed not symmetric candidate: " + nexts(i) + newt.graph)
                      //                      else
                      tiling ++= edges
                      return Success((actualFailed ++ nexts.take(i)).map({
                        case (e, numbers) => (e.toOuter, numbers)
                      }))
                    } //else logger.debug("\nfailed not compatible candidate: " + nexts(i))
                  case _ => throw new Error
                }
              case Failure(e) => //logger.debug("\nfailed candidate: " + nexts(i) + " " + e.getMessage)
            }
          case Failure(e) => //logger.debug("\nfailed candidate: " + nexts(i) + " " + e.getMessage)
        }
      }

      Failure(new Throwable("cannot find suitable candidate"))
    }

    private def expStart: Try[List[Fail]] = Try(Nil)

    def expPatterns(patterns: List[Full],
                     steps: Int,
                     infinite: Boolean = false,
                     mandatory: Boolean = false): Try[Unit] = {
      (0 until steps)
        .foldLeft(expStart)((acc, _) =>
          acc.flatMap(f => tiling.addPatterns(patterns, infinite, failed = f, mandatory)))
        .flatMap({ case _ => Success(()) })
    }

    def scanPatterns(patterns: List[Full],
                      steps: Int,
                      infinite: Boolean = false,
                      mandatory: Boolean = false): List[Try[Tiling]] = {
      val (_, ts) = (0 until steps)
        .foldLeft((expStart, List(Try(tiling))))({
          case ((failed, tilings), _) =>
            failed match {
              case Failure(_) => (failed, tilings)
              case Success(f) =>
                val c = tilings.head.safeGet.clone()
                (c.addPatterns(patterns, infinite, failed = f, mandatory), Try(c) +: tilings)
            }
        })
      ts.reverse
    }

  }

}
