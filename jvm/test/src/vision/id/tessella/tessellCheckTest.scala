package vision.id.tessella

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec

import scalax.collection.Graph

import vision.id.tessella.Alias.Tiling

class tessellCheckTest extends FlatSpec with Methods with Checkers {

  val PolyTessGen: Gen[Tiling] = for {
    sides ← Gen.oneOf(RegPgon.sidesToPgon.keys.toList)
  } yield Tiling.poly(sides)

  "A tessellation made of a single reg p-gon" must "have graph equal to perimeter graph" in {
    check(forAll(PolyTessGen)(t ⇒ t === Tiling.fromG(Graph.from(Nil, t.periEdges))))
  }

  "A tessellation" can "be duplicated" in {
    check(forAll(PolyTessGen)(t ⇒ t === Tiling.fromG(Graph.from(Nil, t.edges))))
  }

}
