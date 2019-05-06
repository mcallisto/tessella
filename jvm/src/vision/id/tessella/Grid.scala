package vision.id.tessella

import scala.annotation.tailrec
import scala.collection.SortedSet

import vision.id.tessella.Cartesian2D.{OrderedUnitSegment2D, Point2D}
import vision.id.tessella.Tau.TAU

trait Grid {

  type Grid = SortedSet[OrderedUnitSegment2D]

  implicit final class GridOps(grid: Grid) {

    /**
      * @param centre     rotation centre
      * @param symmetries number of symmetries
      * @return grids in number equal to the double of symmetries (flipped versions too)
      */
    def rotatedGrids(centre: Point2D = Point2D.origin, symmetries: Int = 12): List[Grid] = {
      val grids = grid +: (1 until symmetries).toList.map(a => grid.map(_.rotate(TAU / symmetries * a, centre)))
      grids ++ grids.map(_.map(_.flipH(centre.x)))
    }

    /**
      * @param set grid
      * @return if grid is an exact part of set
      * @note grid.diff(set).isEmpty would equally check if grid is a subset of set
      *       but this implementation, that relies on sorted segments, might be faster
      *       (it exits at the first element of grid not found in set)
      */
    @tailrec
    def isSubsetOf(set: Grid): Boolean = grid.headOption match {
      case None => true
      case gh =>
        set.headOption match {
          case None => false
          case rh   => (if (gh == rh) grid.tail else grid).isSubsetOf(set.tail)
        }
    }

  }

}
