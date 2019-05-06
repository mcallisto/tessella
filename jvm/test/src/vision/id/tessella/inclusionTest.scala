package vision.id.tessella

import org.scalatest.FlatSpec

import vision.id.tessella.Cartesian2D._

class inclusionTest extends FlatSpec with ListUtils with TryUtils {

  "A polygon" can "be tested if includes a point" in {
    val p = new Polygon(List((-1.0, 2.0), (1.0, 2.0), (0.0, 0.0)).map(new Point2D(_)))
    assert(p.includes(new Point2D(0.0, 1.0)))
    assert(p.includes(new Point2D(0.0, 1.999999)))
    assert(p.includes(new Point2D(0.0, 2.0)) === false)
    assert(p.includes(new Point2D(0.0, 0.000001)))
    assert(p.includes(new Point2D(0.0, 0.0)) === false)
    assert(p.includes(new Point2D(1.0, 0.0)) === false)
  }

  it can "be tested if includes a segment" in {
    val p = new Polygon(List((0.0, 0.0), (4.0, 0.0), (4.0, 4.0), (2.0, 2.0), (0.0, 4.0)).map(new Point2D(_)))
    assert(p.includes(Segment2D.fromCoords2D((1.0, 1.0), (3.0, 1.0))))
    assert(p.includes(Segment2D.fromCoords2D((1.0, 3.0), (3.0, 3.0))) === false)
  }

  "A square" can "be tested if includes a vertex or a side" in {
    val vertices = List((-1.0, 0.0), (-1.0, 1.0), (0.0, 1.0), (0.0, 0.0)).map(new Point2D(_))
    val p = new Polygon(vertices)
    val sides = p.toSegments2D
    // @todo need to understand why
    assert(p.includes(vertices.safeHead) === true)
    assert(sides(2).windingNumber(vertices.safeHead) === -1)
    assert(vertices.tail.forall(!p.includes(_)))
    assert(sides.forall(!p.includes(_)))
  }

}
