package vision.id.tessella

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec

import vision.id.tessella.TessellGraph.Tessell

class tessellCheckTest extends FlatSpec with Checkers {

  val PolyTessGen: Gen[Tessell] = for {
    sides ← Gen.oneOf(RegPgon.sidesToPgon.keys.toList)
  } yield TessellGraph.poly(sides)

  "A tessellation made of a single reg p-gon" must "have graph equal to perimeter graph" in {
    check(forAll(PolyTessGen)(t ⇒ t.graph == t.perimeter))
  }

  "A tessellation" can "be duplicated" in {
    check(forAll(PolyTessGen)(t ⇒ new TessellGraph(t.graph).graph == t.graph))
  }

}
