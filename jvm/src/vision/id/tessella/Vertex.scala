package vision.id.tessella

import scala.util.Try

import vision.id.tessella.Tau.TAU
import vision.id.tessella.Cartesian2D.Point2D
import vision.id.tessella.Polar.{PointPolar, UnitRegularPgon}
import vision.id.tessella.Tessella.Tiling

/**
  * set of ordered adjacent regular p-gons completing a vertex
  */
case class Vertex(pgons: List[RegPgon]) extends AddUtils {

  /**
    * being the 3 p-gon the smallest, and not more than 6 of it fitting
    */
  require(pgons.lengthCompare(6) <= 0, "impossible to have more than 6 reg p-gons sharing a vertex")

  /**
    * verify there are no more p-gons than the available space
    */
  require(alpha <= TAU, "cannot exceed full angle")

  /**
    * shorten description if consecutive identical elements
    * (⬣.⬣.▲.▲) becomes (⬣².▲²)
    *
    * @return
    */
  override def toString: String = {
    val endNotGroupable: List[String] = minor.pgons.map(_.toString) :+ "?"
    val (descriptions, _): (List[String], List[String]) = endNotGroupable
      .foldLeft((List(): List[String], List(): List[String]))({
        case ((descs, groupable), d) =>
          if (groupable.isEmpty || d == groupable.safeHead) (descs, d +: groupable)
          else
            (descs :+ (groupable match {
              case single :: Nil => single
              case _             => groupable.safeHead + RegPgon.sups(groupable.size)
            }), List(d))
      })
    "(" + descriptions.mkString(".") + ")"
  }

  /**
    * number of edges of each p-gon
    *
    * @return
    */
  def edgesNumbers: List[Int] = pgons.map(_.edgesNumber)

  /**
    * interior angle
    *
    * @return radians
    */
  def alpha: Double = pgons.map(_.alpha).sum

  /**
    * exterior angle
    *
    * @return radians
    */
  def beta: Double = TAU - alpha

  /**
    * @return
    */
  def allVersions: IndexedSeq[Vertex] = pgons.reflections.distinct.map(Vertex(_))

  def minor: Vertex = allVersions.min

  def isEquivalentTo(that: Vertex): Boolean = this.pgons.isReflectionOf(that.pgons)

  def isFull: Boolean = alpha ~= TAU

  def toPoint2Ds(angle: Double = 0.0): List[Point2D] = {
    val angles = pgons.scanLeft(angle)(_ + _.alpha)
    (if (isFull) angles.init else angles).map(angle => new PointPolar(1.0, angle).toPoint2D)
  }

  def distinct: Vertex = Vertex(pgons.distinct)

  def isReflectionOf(that: Vertex): Boolean = this.pgons.isReflectionOf(that.pgons)

  def append(pgon: RegPgon): Try[Vertex] = Try(new Vertex(pgons :+ pgon))

  def prepend(pgon: RegPgon): Try[Vertex] = Try(new Vertex(pgon +: pgons))

  def isContainedIn(full: Full): Boolean =
    full.pgons.rotaReflections.distinct.map(_.take(this.pgons.size)).distinct.contains(this.pgons)

  def isContainedIn(that: Vertex): Boolean =
    if (that.isFull) this.isContainedIn(new Full(that.pgons))
    else that.pgons.reflections.distinct.flatMap(_.sliding(this.pgons.size)).distinct.contains(this.pgons)

  def merge(that: Vertex): List[Vertex] =
    this match {
      case equal if equal == that                                     => List(that)
      case smaller if smaller.isContainedIn(that)                     => List(that)
      case bigger if that.isContainedIn(bigger)                       => List(this)
      case full if full.isFull || that.isFull                         => Nil
      case single if single.pgons.size == 1 || that.pgons.size == 1 => Nil
      case _ =>
        val overlaps: IndexedSeq[List[RegPgon]] = for {
          index <- 1 until Math.min(this.pgons.size, that.pgons.size)
          at    <- that.pgons.reflections
          is    <- this.pgons.reflections
          if at.take(index) == is.takeRight(index)
        } yield is ++ at.drop(index)
        val vertices = overlaps.map(p => Try(new Vertex(p))).filter(_.isSuccess).map(_.safeGet)
        vertices.map(v => if (v.isFull) new Full(v.pgons) else v).map(_.minor).distinct.toList.sorted
    }

  private def regPgonTiling(edgesNumber: Int): Try[Tiling] = RegPgon.ofEdges(edgesNumber).map(_.toTiling)

  /** grow tessellation from vertex
    */
  def toTiling: Tiling = pgons match {
    case pgon :: ps =>
      val t: Tiling = pgon.toTiling
      ps.foldLeft(pgon.edgesNumber)({
        case (count, p) =>
          t.addToEdge(Side(1, count), p).safeGet
          count + p.edgesNumber - 2
      })
      t
    case Nil => Tiling.fromSides(Set())
  }

  /** all tessellations grown from start to given vertex adding one p-gon at a time
    */
  def toScan: List[Tiling] = pgons match {
    case pgon :: ps =>
      val t: Tiling = pgon.toTiling
      val (ts, _) = ps
        .foldLeft(List(t), pgon.edgesNumber)({
          case ((tilings, count), p) =>
            val c = tilings.safeHead.clone()
            c.addToEdge(Side(1, count), p).safeGet
            (c +: tilings, count + p.edgesNumber - 2)
        })
      ts.reverse
    case Nil => Nil
  }

}

object Vertex extends Symmetry with DistinctUtils[List[UnitRegularPgon]] with MathUtils with TryUtils {

  implicit def orderingByRegPgon[A <: Vertex]: Ordering[A] = (a: A, b: A) => RegPgon.listCompare(a.pgons, b.pgons)

  private def fromTryRegPgon(tr: Try[List[RegPgon]]): Try[Vertex] = tr.flatMap(ps => Try(Vertex(ps)))

  /**
    * create vertex with p-gons of given sides
    *
    * @param edgesNumbers numbers of sides of each pgon
    * @return
    */
  def fromEdgesNumbers(edgesNumbers: List[Int]): Try[Vertex] =
    fromTryRegPgon(RegPgon.sequence(edgesNumbers.map(RegPgon.ofEdges)))

  def p(edgesNumbers: List[Int]): Vertex = fromEdgesNumbers(edgesNumbers).safeGet

  def fromString(s: String): Try[Vertex] = fromTryRegPgon(RegPgon.fromStrings(s))

  def s(s: String): Vertex = fromString(s).safeGet

  /**
    * find all combinations of regular polygons joined at 1 vertex filling an angle
    *
    * @param l list of regular polygons by side
    * @param a angle to be filled (default 360°)
    * @param withPermutations if true add distinct (non symmetric) permutations
    * @return
    */
  def fillAngle(l: List[Int], a: Double = TAU, withPermutations: Boolean = false): List[Vertex] = {
    require(l == l.distinct, "must be distinct")
    require(l == l.sorted, "must be sorted")

    /**
      * @param theta   angle
      * @param ps  p-gons to be tested, ordered by increasing number of sides
      * @param acc accumulator
      * @return combination of polygons
      */
    def loop(theta: Double, ps: List[UnitRegularPgon], acc: List[List[UnitRegularPgon]]): List[List[UnitRegularPgon]] =
      if (ps.isEmpty) acc
      else {
        ps.filter(_.alpha <~= theta)
          .flatMap(x =>
            theta - x.alpha match {
              case zero if zero ~= 0.0 => acc.map(_ :+ x)
              case angle               => loop(angle, ps.filter(_.alpha <~= x.alpha), acc.map(_ :+ x))
          })
      }

    val vs = loop(a, l.map(UnitRegularPgon.ofEdges), List(List()))
    val urps =
      if (withPermutations) vs.flatMap(_.permutations.toList.distinctBy(_.isRotationOrReflectionOf(_)))
      else vs
    urps.map(urp => Vertex.p(urp.map(_.n)))
  }

}
