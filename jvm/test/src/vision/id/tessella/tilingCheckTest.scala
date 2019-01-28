package vision.id.tessella

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec

import vision.id.tessella.Tessella.Tiling

class tilingCheckTest extends FlatSpec with TilingUtils with TryUtils with Checkers {

  val polyTessGen: Gen[Tiling] = for {
    sides <- Gen.oneOf(RegPgon.edgesNumberToPgon.keys.toList)
  } yield RegPgon.ofEdges(sides).map(_.toTiling).safeGet

  "A tessellation made of a single reg p-gon" must "have graph equal to perimeter graph" in {
    check(forAll(polyTessGen)(t => t === Tiling.fromSides(t.perimeterOrderedEdges.toSet)))
  }

  "A tessellation" can "be duplicated" in {
    check(forAll(polyTessGen)(t => t === Tiling.fromSides(t.edges.map(_.toOuter).toSet)))
  }

}
