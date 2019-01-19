package vision.id.tessella

import scala.xml._
import os.RelPath

import vision.id.tessella.Alias.Tiling
import vision.id.tessella.Cartesian2D._

trait SVG extends TilingUtils {

  val multiple: Double = 25.0

  def wrap(xml: NodeBuffer): Elem =
    <svg xmlns="http://www.w3.org/2000/svg" version="1.1">
      {xml}
    </svg>

  val fillMap: Map[Int, String] = Map(
    3  -> "yellow",
    4  -> "goldenrod",
    5  -> "hotpink",
    6  -> "grey",
    7  -> "orange",
    8  -> "coral",
    9  -> "darkgray",
    10 -> "lightpink",
    12 -> "lightgreen",
    15 -> "darkseagreen",
    18 -> "olivedrab",
    20 -> "olive",
    24 -> "palegreen",
    42 -> "darkolivegreen"
  )

  val marksColor: List[String] = List(
    "blue",
    "dark_blue",
    "cyan",
    "black",
    "purple",
    "turquoise"
  )

  def saveFilePretty(xml: NodeBuffer, filename: String): Unit = {
    val prettyPrinter = new PrettyPrinter(80, 2)
    val prettyXml     = prettyPrinter.format(wrap(xml))
    val target        = os.Path(RelPath(filename + ".svg"), os.pwd)
    os.write.over(target, "<?xml version='1.0' encoding='UTF-8'?>\n" + prettyXml)
  }

  def saveFile(xml: NodeBuffer, filename: String): Unit = XML.save(
    filename + ".svg",
    wrap(xml),
    "UTF-8",
    xmlDecl = true
  )

  def prepare[T](ts: List[T], f: T => Elem): NodeBuffer =
    ts.map(f).foldLeft(new NodeBuffer)(_ &+ _)

  def addStyle(e: Elem, style: String): Elem =
    if (style.isEmpty) e
    else e % new UnprefixedAttribute("style", style, Null)

  def group(xml: NodeBuffer, style: String): Elem =
    addStyle(<g>
      {xml}
    </g>,
             style)

  /**
    * point where to move the p-gon to have it in the positive quadrant starting at origin
    *
    * @param p p-gon
    * @return
    */
  private def getDiff(p: Polyline2D): Point2D = {
    val (min, _) = p.getMinMax
    min
      .scale(-1.0) //equal to new Point2D((-min.x, -min.y))
      .move(0.08, 0.08) // nicer with margin
  }

  // compensate for fonts, more centered on node
  private def prettyLabel(l: Label2D): Label2D = l.move(-0.12, 0.1)

  def draw(t: Tiling,
           perim: Boolean = true,
           polys: Boolean = false,
           labelStyle: Int = 1,
           markStyle: Int = 0): NodeBuffer = {
    val tm   = t.toNodesMap
    val p    = t.toPerimeterPolygon(tm)
    val diff = getDiff(p)
    val grid = getGrid(t.toSegments2D(tm), diff)
    val perimeter: NodeBuffer =
      if (perim) new NodeBuffer() &+ getPerimeter(p, diff)
      else new NodeBuffer()
    val polygons: NodeBuffer =
      if (polys) new NodeBuffer() &+ getPolygons(t.toPolygons(tm), diff)
      else new NodeBuffer()
    val labels: NodeBuffer = labelStyle match {
      case 1 => new NodeBuffer() &+ getLabels(t.toPerimeterLabels2D(tm), diff)
      case 2 => new NodeBuffer() &+ getLabels(t.toLabels2D(tm), diff)
      case _ => new NodeBuffer()
    }
    val marks: Any = markStyle match {
      case 1 => new NodeBuffer() &+ getGonalMarks(t.toGonals(tm), diff)
      case _ => new NodeBuffer()
    }
    new NodeBuffer() &+ grid &+ polygons &+ perimeter &+ marks &+ labels
  }

  def getGrid(g: List[Segment2D], diff: Point2D): Elem =
    group(prepare[Segment2D](
            g,
            _.sum(diff).scale(multiple).toSVG(style = "")
          ),
          "stroke:black")

  private def getPerimeter(p: Polygon, diff: Point2D): Elem =
    p.sum(diff).scale(multiple).toSVG("fill:none;stroke:blue;stroke-width:2")

  private def getPolygons(ps: List[Polygon], diff: Point2D): Elem = {
    val grouped = ps.groupBy(_.cs.size)
    group(
      prepare(
        grouped.keys.toList.map(
          k =>
            group(prepare[Polygon](
                    grouped(k),
                    _.sum(diff).scale(multiple).toSVG("stroke:none")
                  ),
                  "fill:" + fillMap(k))),
        (e: Elem) => e
      ),
      style = ""
    )

  }

  def getLabels(l: List[Label2D], diff: Point2D): Elem =
    group(prepare[Label2D](
            l,
            prettyLabel(_).sum(diff).scale(multiple).toSVG(style = "")
          ),
          "fill:red")

  def getGonalMarks(l: List[List[Point2D]], diff: Point2D): Elem =
    group(
      prepare(
        l.indices.toList.map(
          i =>
            group(prepare[Point2D](
                    l(i),
                    p => new Circle(p.c, multiple * 0.1).sum(diff).scale(multiple).toSVG(style = "")
                  ),
                  "fill:" + marksColor(i % 6))),
        (e: Elem) => e
      ),
      ""
    )

}
