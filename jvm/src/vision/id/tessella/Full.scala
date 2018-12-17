package vision.id.tessella

import scala.util.Try

/**
  * set of circularly ordered adjacent regular p-gons completing a vertex
  */
class Full(ps: List[RegPgon]) extends Vertex(ps) with Symmetry {

  require(isFull, "must create a full angle")

  override def toString: String = new Vertex(minor.ps).toString

  override def allVersions: IndexedSeq[Full] = this.ps.rotaReflections.distinct.map(new Full(_))

  override def minor: Full = allVersions.min

  def isEquivalentTo(that: Full): Boolean = this.ps.isRotationOrReflectionOf(that.ps)

}

object Full {

  private def fromTryRegPgon(tr: Try[List[RegPgon]]): Try[Full] = tr.flatMap(ps ⇒ Try(new Full(ps)))

  /**
    * create full vertex with p-gons of given sides
    *
    * @param psSides numbers of sides
    * @return
    */
  def fromSides(psSides: List[Int]): Try[Full] =
    fromTryRegPgon(RegPgon.sequence(psSides.map(sides ⇒ RegPgon.ofSides(sides))))

  def p(psSides: List[Int]): Full = fromSides(psSides).get

  def s(s: String): Full = new Full(Vertex.fromString(s).get.ps)

}
