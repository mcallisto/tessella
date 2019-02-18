package vision.id.tessella

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import vision.id.tessella.Tessella.Tiling
import vision.id.tessella.Polar.UnitRegularPgon

sealed abstract class RegPgon(val edgesNumber: Int, val symbol: Option[Char]) extends Ordered[RegPgon] {

  def compare(that: RegPgon): Int = this.edgesNumber - that.edgesNumber

  override val toString: String = symbol match {
    case Some(s) => s.toString
    case None    => edgesNumber.toString
  }

  val alpha: Double = UnitRegularPgon.ofEdges(edgesNumber).alpha

  def toTiling: Tiling =
    Tiling.fromSides((1 to edgesNumber).toSet.map((i: Int) => Side(i, i % edgesNumber + 1, isPerimeter = Some(true))))

}

case object Triangle  extends RegPgon(3, Some('▲'))
case object Square    extends RegPgon(4, Some('■'))
case object Pentagon  extends RegPgon(5, Some('⬟'))
case object Hexagon   extends RegPgon(6, Some('⬣'))
case object Eptagon   extends RegPgon(7, None)
case object Octagon   extends RegPgon(8, Some('⯃'))
case object Ennagon   extends RegPgon(9, None)
case object Decagon   extends RegPgon(10, None)
case object Dodecagon extends RegPgon(12, None)
case object Gon15     extends RegPgon(15, None)
case object Gon18     extends RegPgon(18, None)
case object Icosagon  extends RegPgon(20, None)
case object Gon24     extends RegPgon(24, None)
case object Gon42     extends RegPgon(42, None)

object RegPgon extends TryUtils with ListUtils {

  val all: List[RegPgon] = List(
    Triangle,
    Square,
    Pentagon,
    Hexagon,
    Eptagon,
    Octagon,
    Ennagon,
    Decagon,
    Dodecagon,
    Gon15,
    Gon18,
    Icosagon,
    Gon24,
    Gon42
  )

  val edgesNumberToPgon: Map[Int, RegPgon] = all.map(pgon => (pgon.edgesNumber, pgon)).toMap

  val symbolToPgon: Map[Char, RegPgon] =
    all.filter(_.symbol.isDefined).map(pgon => (pgon.symbol.safeGet(), pgon)).toMap

  def ofEdges(edgesNumber: Int): Try[RegPgon] = edgesNumberToPgon.get(edgesNumber) match {
    case Some(pgon) => Success(pgon)
    case None       => Failure(throw new IllegalArgumentException("non-tiling edges number: " + edgesNumber))
  }

  def fromString(s: String): Try[RegPgon] = s.toList match {
    case h :: Nil if symbolToPgon.contains(h) => Success(symbolToPgon(h))
    case _                                    => Try(s.trim.toInt).flatMap(edgesNumber => ofEdges(edgesNumber))
  }

  val sups: Map[Int, Char] = Map(
    2 -> '²',
    3 -> '³',
    4 -> '⁴',
    5 -> '⁵',
    6 -> '⁶'
  )

  val supsInv: Map[Char, Int] = sups.map({ case (k, v) => (v, k) })

  def fromStrings(s: String): Try[List[RegPgon]] = s.toList match {
    case '(' :: t =>
      t match {
        case ss =>
          if (ss.isEmpty || ss.safeLast != ')')
            Failure(throw new IllegalArgumentException("must end with )"))
          else
            ss.init match {
              case Nil => Success(List())
              case centre =>
                val l: List[String] = centre.mkString
                  .split('.')
                  .toList
                  .flatMap(d =>
                    d.indexOf('*') match {
                      case -1 =>
                        supsInv.get(d.last) match {
                          case Some(sup) => List.fill(sup)(d.init)
                          case None      => List(d)
                        }
                      case i => List.fill(d.drop(i + 1).toInt)(d.take(i))
                  })
                sequence(l.foldLeft(Nil: List[Try[RegPgon]])((acc, r) => acc :+ fromString(r)))
            }
      }
    case _ => Failure(throw new IllegalArgumentException("must start with ("))
  }

  @tailrec
  def listCompare(a: List[RegPgon], b: List[RegPgon]): Int = b match {
    case Nil if a.isEmpty => 0
    case Nil              => 1
    case hb :: tb =>
      a match {
        case Nil      => -1
        case ha :: ta => if (ha == hb) listCompare(ta, tb) else ha.compare(hb)
      }
  }

}
