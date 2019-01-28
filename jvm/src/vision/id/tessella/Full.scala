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

  /**
    * find every distinct p-gon that could be the next addition to vertex in order to become full
    *
    * @param vertex vertex
    * @param reversed direction
    * @return
    */
  def findNexts(vertex: Vertex, reversed: Boolean = false): List[Int] = {
    val number = vertex.pgonsNumber
    if (this.pgonsNumber <= number) List()
    else {
      val previous = vertex.ps match {
        case rev if reversed => rev.reverse
        case std => std
      }
      allVersions
        .filter(_.ps.take(number) == previous) // only those whose beginning matches
        .map(_.edgesNumbers(number)) // take next p-gon fitting
        .distinct.toList
    }
  }
}

object Full extends TryUtils {

  def distinct(patterns: List[Full]): List[Full] = patterns.map(_.minor).distinct

  private def fromTryRegPgon(tr: Try[List[RegPgon]]): Try[Full] = tr.flatMap(ps => Try(new Full(ps)))

  /**
    * create full vertex with p-gons of given edges
    *
    * @param fromEdgesNumbers numbers of edges
    * @return
    */
  def fromEdgesNumbers(edgesNumbers: List[Int]): Try[Full] =
    fromTryRegPgon(RegPgon.sequence(edgesNumbers.map(RegPgon.ofEdges)))

  def p(edgesNumbers: List[Int]): Full = fromEdgesNumbers(edgesNumbers).safeGet

  def s(s: String): Full = new Full(Vertex.fromString(s).safeGet.ps)

  /**
    * able to form edge-to-edge 1-uniform, monohedral tilings by regular polygons
    */
  val regularPatterns: List[Full] = List(
    "(▲⁶)",
    "(■⁴)",
    "(⬣³)"
  ).map(s)

  /**
    * able to form edge-to-edge 1-uniform, 2 or 3-hedral tilings by regular polygons
    */
  val semiRegularPatterns: List[Full] = List(
    "(▲⁴.⬣)",
    "(▲³.■²)",
    "(▲².■.▲.■)",
    "(▲.■.⬣.■)",
    "(▲.⬣.▲.⬣)",
    "(▲.12²)",
    "(■.⬣.12)",
    "(■.⯃²)"
  ).map(s)

  /**
    * able to be part of edge-to-edge k-uniform tilings by regular polygons
    */
  val otherRegularPatterns: List[Full] = List(
    "(▲².■.12)",
    "(▲².⬣²)",
    "(▲.■.▲.12)",
    "(▲.■².⬣)"
  ).map(s)

  /**
    * not able to form or be part of tilings by regular polygons
    */
  val notExtensiblePatterns: List[Full] = List(
    "(▲.7.42)",
    "(▲.⯃.24)",
    "(▲.9.18)",
    "(▲.10.15)",
    "(■.⬟.20)",
    "(⬟².10)"
  ).map(s)

  def extensiblePatterns: List[Full] = regularPatterns ++ semiRegularPatterns.init ++ otherRegularPatterns
}
