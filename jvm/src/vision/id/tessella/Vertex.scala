package vision.id.tessella

import scala.util.Try

import vision.id.tessella.Tau.τ
import vision.id.tessella.Cartesian2D.Point2D
import vision.id.tessella.Polar.{PointPolar, UnitRegularPgon}

/**
  * set of ordered adjacent regular p-gons completing a vertex
  */
case class Vertex(ps: List[RegPgon]) extends /*Ordered[Vertex] with */ Symmetry with MathUtils {

  /**
    * being the 3 p-gon the smallest, and not more than 6 of it fitting
    */
  require(ps.lengthCompare(6) <= 0, "impossible to have more than 6 reg p-gons sharing a vertex")

  /**
    * verify there are no more p-gons than the available space
    */
  require(α <= τ, "cannot exceed full angle")

  /**
    * shorten description if consecutive identical elements
    * (⬣.⬣.▲.▲) becomes (⬣².▲²)
    *
    * @return
    */
  override def toString: String = {
    val descriptions: List[String] = (minor.ps.map(_.toString) :+ "?") // add not groupable element at the end
      .foldLeft((List(): List[String], List(): List[String]))({
        case ((descs, groupable), d) ⇒
          if (groupable.isEmpty || d == groupable.head) (descs, d +: groupable)
          else
            (descs :+ (groupable match {
              case single :: Nil ⇒ single
              case _             ⇒ groupable.head + RegPgon.sups(groupable.size)
            }), List(d))
      })
      ._1
    "(" + descriptions.mkString(".") + ")"
  }

  def size: Int = ps.size

  /**
    * sides of p-gons
    *
    * @return
    */
  def psSides: List[Int] = ps.map(_.sides)

  /**
    * interior angle
    *
    * @return radians
    */
  def α: Double = ps.foldLeft(0.0)(_ + _.α)

  /**
    * exterior angle
    *
    * @return radians
    */
  def β: Double = τ - α

  /**
    * @return
    */
  def allVersions: IndexedSeq[Vertex] = ps.reflections.distinct.map(Vertex(_))

  def minor: Vertex = allVersions.min

  def isEquivalentTo(that: Vertex): Boolean = this.ps.isReflectionOf(that.ps)

  def isFull: Boolean = α ≈ τ

  def toPoint2Ds(angle: Double = 0.0): List[Point2D] = {
    val angles = ps.scanLeft(angle)(_ + _.α)
    (if (isFull) angles.init else angles).map(angle ⇒ new PointPolar(1.0, angle).toPoint2D)
  }

  def distinct: Vertex = Vertex(ps.distinct)

  def isReflectionOf(that: Vertex): Boolean = this.ps.isReflectionOf(that.ps)

}

object Vertex extends Symmetry with DistinctUtils[List[UnitRegularPgon]] with MathUtils {

  implicit def orderingByRegPgon[A <: Vertex]: Ordering[A] = (a: A, b: A) ⇒ RegPgon.listCompare(a.ps, b.ps)

  private def fromTryRegPgon(tr: Try[List[RegPgon]]): Try[Vertex] = tr.flatMap(ps ⇒ Try(Vertex(ps)))

  /**
    * create vertex with p-gons of given sides
    *
    * @param psSides numbers of sides of each pgon
    * @return
    */
  def fromSides(psSides: List[Int]): Try[Vertex] = fromTryRegPgon(RegPgon.sequence(psSides.map(n ⇒ RegPgon.ofSides(n))))

  def p(psSides: List[Int]): Vertex = fromSides(psSides).get

  def fromString(s: String): Try[Vertex] = fromTryRegPgon(RegPgon.fromStrings(s))

  def s(s: String): Vertex = fromString(s).get

  /**
    * find all combinations of regular polygons joined at 1 vertex filling an angle
    *
    * @param l list of regular polygons by side
    * @param a angle to be filled (default 360°)
    * @param withPermutations if true add distinct (non symmetric) permutations
    * @return
    */
  def fillAngle(l: List[Int], a: Double = τ, withPermutations: Boolean = false): List[Vertex] = {
    require(l == l.distinct, "must be distinct")
    require(l == l.sorted, "must be sorted")

    /**
      * @param θ   angle
      * @param ps  p-gons to be tested, ordered by increasing number of sides
      * @param acc accumulator
      * @return combination of polygons
      */
    def loop(θ: Double, ps: List[UnitRegularPgon], acc: List[List[UnitRegularPgon]]): List[List[UnitRegularPgon]] =
      if (ps.isEmpty) acc
      else {
        ps.filter(_.α ⪅ θ)
          .flatMap(x ⇒
            θ - x.α match {
              case zero if zero ≈ 0.0 ⇒ acc.map(_ :+ x)
              case angle              ⇒ loop(angle, ps.filter(_.α ⪅ x.α), acc.map(_ :+ x))
          })
      }

    val vs = loop(a, l.map(UnitRegularPgon.ofSides), List(List()))
    val urps =
      if (withPermutations) vs.flatMap(_.permutations.toList.distinctBy(_.isRotationOrReflectionOf(_)))
      else vs
    urps.map(urp ⇒ Vertex.p(urp.map(_.n)))
  }

}
