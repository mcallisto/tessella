package vision.id.tessella

import org.scalatest.FlatSpec

import scala.xml.NodeBuffer

class SVGTest extends FlatSpec with SVG {

  val p = new scala.xml.PrettyPrinter(80, 2)

  "An SVG elem" can "be created wrapping XML nodes" in {
    val nb = new NodeBuffer()
    assert(
      p.format(wrap(nb &+ <foo></foo> &+ <bar></bar>)) ===
        """<svg version="1.1" xmlns="http://www.w3.org/2000/svg">
        |  <foo></foo>
        |  <bar></bar>
        |</svg>""".stripMargin)
  }

}
