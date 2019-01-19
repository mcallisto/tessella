package vision.id.tessella

import vision.id.tessella.Alias.Tiling
import vision.id.tessella.Polar.RegularPgon
import vision.id.tessella.Tau.TAU

import scala.util.{Success, Try}

trait AddUtils extends TilingUtils with MathUtils {

  final implicit class ATiling(tiling: Tiling) {

    private implicit class ANode(node: tiling.NodeT) {

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

      private def orderedIndex: Int = tiling.perimeterOrderedNodes.indexOf(node.toOuter)

      def startVertexes(dir: Boolean = true): List[Vertex] = tiling.vertexes.rotateWithDirection(-orderedIndex, dir)

      /**
        * get the node where to attach a p-gon, it can move because of adjacent p-gons
        *
        * @param a   internal angle of the p-gon to be attached
        * @param dir direction along the perimeter, true is following perimeterOrderedNodes, false other way
        * @return id and edges to be subtracted from addition
        */
      def getAttachNode(a: Double, dir: Boolean): Try[(Int, List[Side[Int]])] = Try {
        val ns = node.startNodes(dir)
        val vs = node.startVertexes(dir)
        vs.indices.foldLeft(Nil: List[Side[Int]])((add, i) =>
          vs(i).alpha match {
            case adjacent if a + adjacent ~= TAU => add :+ Side(ns(i), ns(i + 1)) // go to next
            case toomuch if a + toomuch > TAU    => throw new IllegalArgumentException("angle more than full, dir: " + dir)
            case _                               => return Try(ns(i), add)
        })
        throw new IllegalArgumentException("no more sides")
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
                    Success((newPerimeterEdges, backOldPerimeterEdges ++ forwardOldPerimeterEdges))
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

    }

    // ----------------- addition -------------------

    def addToEdgePgon(edge: Side[Int], edgesNumber: Int): Try[Tiling] = (tiling get edge).addPgonOfEdges(edgesNumber)

  }

  /**
    * get new ids for p-gons to be added
    *
    * @param n          number of new ids needed
    * @param emptiesMax max id and unused ones in between
    * @return
    */
  def getFreeIds(n: Int, emptiesMax: (List[Int], Int)): List[Int] = n match {
    case e if e < 0 => throw new Error
    case 0          => List()
    case _ =>
      val (es, max) = emptiesMax
      val s         = es.size
      if (s >= n) es.take(n)
      else es ++ ((max + 1) to (max + n - s)).toList
  }

}
