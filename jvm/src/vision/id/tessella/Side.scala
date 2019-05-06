package vision.id.tessella

import scalax.collection.GraphEdge.{EdgeCopy, EdgeLike, NodeProduct, UnDiEdge}
import scalax.collection.GraphPredef.OuterEdge

case class Side[+N](vertex1: N, vertex2: N, var isPerimeter: Option[Boolean] = None)
    extends UnDiEdge[N](NodeProduct(vertex1, vertex2))
    with EdgeCopy[Side]
    with OuterEdge[N, Side] {

  private def this(nodes: Product, isPerimeter: Option[Boolean]) {
    this(nodes.productElement(0).asInstanceOf[N], nodes.productElement(1).asInstanceOf[N], isPerimeter)
  }

  override def copy[NN](newNodes: Product) = new Side[NN](newNodes, isPerimeter)

  override protected def nodesToStringSeparator: String = isPerimeter match {
    case Some(true)  => "-"
    case Some(false) => "="
    case None        => EdgeLike.nodeSeparator
  }

}

object Side extends ListUtils {

  def fromEdge[A](edge: UnDiEdge[A], isPerimeter: Option[Boolean] = None): Side[A] =
    edge.toList.onlyTwoElements(new Side[A](_, _, isPerimeter))

}
