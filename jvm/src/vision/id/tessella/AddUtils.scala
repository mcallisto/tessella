package vision.id.tessella

import scala.annotation.tailrec
import scala.util.{Success, Try}

import vision.id.tessella.Tessella.Tiling
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
        def loop(vertices: List[Vertex], builder: tiling.PathBuilder, index: Int): (Int, List[tiling.EdgeT]) =
          vertices match {
            case Nil => throw new IllegalArgumentException("no more sides")
            case h :: t =>
              h.alpha match {
                case adjacent if a + adjacent ~= TAU =>
                  loop(t, builder += tiling.get(ns(index + 1)), index + 1) // go to next
                case over if a + over > TAU =>
                  throw new IllegalArgumentException("angle more than full, dir: " + dir)
                case _ => (ns(index), builder.result().edges.toList)
              }
          }

        Try(loop(node.startVertexes(dir), tiling.newPathBuilder(node), 0))

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
        * @param p reg p-gon to be added
        * @return
        */
      def additionalEdges(p: RegPgon): Try[(List[Side[Int]], List[Side[Int]])] = {
        val (f, s) = edge.orderedEndPoints
        val angle  = p.alpha
        f.getAttachNode(angle, dir = false)
          .flatMap({
            case (back_node, backOldPerimeterEdges) =>
              s.getAttachNode(angle, dir = true)
                .flatMap({
                  case (forward_node, forwardOldPerimeterEdges) =>
                    val ids: List[Int] =
                      getFreeIds(p.edgesNumber - 2 - backOldPerimeterEdges.size - forwardOldPerimeterEdges.size,
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
        * @param p reg p-gon to be added
        * @return
        */
      def attach(p: RegPgon): Try[Unit] =
        edge.additionalEdges(p).flatMap({ case (newPerimeterEdges, _) => Try(tiling ++= newPerimeterEdges) })

    }

    def addToEdge(edge: Side[Int], p: RegPgon): Try[Unit] = (tiling get edge).attach(p)

  }

}
