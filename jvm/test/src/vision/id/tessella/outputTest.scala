package vision.id.tessella

import scala.util.{Failure, Success}

import org.scalatest.FlatSpec
import org.scalameter._

import vision.id.tessella.Tessella.Tiling

class outputTest extends FlatSpec with GraphUtils with SVG {

//  "The images in the docs/ folder" can "be created" in {
//    assert(docs().isInstanceOf[Unit])
//  }

//  "The images in the out/jvm/test/myAlgos/ folder" can "be created" in {
//    assert(algos().isInstanceOf[Unit])
//  }

//  "The images in the out/jvm/test/myAlgos/growth folder" can "be created" in {
//    assert(growth().isInstanceOf[Unit])
//  }

//  "The test images in the out/jvm/test/myTest/scan folder" can "be created" in {
//    assert(scan().isInstanceOf[Unit])
//  }

//  "The test image in the out/jvm/test/myTest/ folder" can "be created" in {
//    assert(test().isInstanceOf[Unit])
//  }

  def m: Map[String, Tiling] = Map(
    "regular/(▲⁶)"                               -> Tiling.triangleNet(24, 16),
    "regular/(■⁴)"                               -> Tiling.squareNet(8, 8),
    "regular/(⬣³)"                               -> Tiling.hexagonNet(8, 8),
    "uniform/(▲⁴.⬣)"                             -> Tiling.uniform2(24, 16),
    "uniform/(▲³.■²)"                            -> Tiling.elongatedTriangular(12, 12),
    "uniform/(▲.⬣.▲.⬣)"                          -> Tiling.uniform(24, 16),
    "2-uniform/(▲⁶; ▲⁴.⬣)"                       -> Tiling.twoUniform2(8, 8),
    "2-uniform/(▲⁶; ▲⁴.⬣)_alt"                   -> Tiling.twoUniform3(24, 16),
    "2-uniform/(▲⁶; ▲².⬣²)"                      -> Tiling.twoUniform(8, 8),
    "2-uniform/(▲⁴.⬣; ▲².⬣²)"                    -> Tiling.twoUniform4(24, 16),
    "2-uniform/(▲.⬣.▲.⬣; ▲².⬣²)"                 -> Tiling.twoUniform5(24, 16),
    "3-uniform/(▲⁶; ⬣³; ▲².⬣²)"                  -> Tiling.threeUniformOneOneOne(8, 8),
    "3-uniform/(▲⁶; ▲⁴.⬣; ▲².⬣²)"                -> Tiling.threeUniformOneOneOne2(8, 8),
    "3-uniform/(▲⁶; ▲⁴.⬣; ▲².⬣²)_alt"            -> Tiling.threeUniformOneOneOne3(8, 8),
    "3-uniform/(▲⁶; ▲⁴.⬣; ▲.⬣.▲.⬣)"              -> Tiling.threeUniformOneOneOne6(24, 16),
    "3-uniform/(▲⁴.⬣; ▲³.■²; ▲.■².⬣)"            -> Tiling.threeUniformOneOneOne7(12, 12),
    "3-uniform/(▲³.■²; ▲².⬣²; ▲.■².⬣)"           -> Tiling.threeUniformOneOneOne8(12, 12),
    "3-uniform/(▲².⬣²; ▲.⬣.▲.⬣; ⬣³)"             -> Tiling.threeUniformOneOneOne4(24, 16),
    "3-uniform/(▲².⬣²; ▲.⬣.▲.⬣; ⬣³)_alt"         -> Tiling.threeUniformOneOneOne5(24, 16),
    "3-uniform/([2x ▲⁶]; ▲⁴.⬣)"                  -> Tiling.threeUniformTwoOne(8, 8),
    "4-uniform/(▲⁶; ▲⁴.⬣; ▲².⬣²; ⬣³)"            -> Tiling.fourUniformOneOneOneOne(8, 8),
    "4-uniform/(▲⁶; ▲⁴.⬣; ▲².⬣²; ⬣³)_alt"        -> Tiling.fourUniformOneOneOneOne1(8, 8),
    "4-uniform/(▲⁶; ▲⁴.⬣; ▲².⬣²; ⬣³)_alt2"       -> Tiling.fourUniformOneOneOneOne2(8, 8),
    "4-uniform/(▲⁶; [2x ⬣³]; ▲².⬣²)"             -> Tiling.fourUniformTwoOneOne(8, 8),
    "4-uniform/(▲⁶; [2x ⬣³]; ▲².⬣²)_alt"         -> Tiling.fourUniformTwoOneOne2(8, 8),
    "4-uniform/(▲⁶; ▲².⬣²; [2x ⬣³])_alt2"        -> Tiling.fourUniformTwoOneOne3(8, 8),
    "4-uniform/(▲⁶; [2x ▲².⬣²]; ⬣³)"             -> Tiling.fourUniformTwoOneOne4(8, 8),
    "4-uniform/(▲⁶; [2x ▲².⬣²]; ⬣³)_alt"         -> Tiling.fourUniformTwoOneOne5(8, 8),
    "4-uniform/(▲⁶; ▲².⬣²; [2x ⬣³])_alt3"        -> Tiling.fourUniformTwoOneOne6(8, 8),
    "4-uniform/(▲⁶; [2x ▲⁴.⬣]; ▲².⬣²)"           -> Tiling.fourUniformTwoOneOne7(8, 8),
    "4-uniform/([2x ▲⁶]; ▲⁴.⬣; ▲².⬣²)"           -> Tiling.fourUniformTwoOneOne8(24, 16),
    "5-uniform/([2x ▲⁶]; ▲⁴.⬣; ▲².⬣²; ⬣³)"       -> Tiling.fiveUniformTwoOneOneOne(8, 8),
    "5-uniform/(▲⁶; ▲⁴.⬣; ▲².⬣²; [2x ⬣³])"       -> Tiling.fiveUniformTwoOneOneOne2(8, 8),
    "5-uniform/(▲⁶; ▲⁴.⬣; [2x ▲².⬣²]; ⬣³)"       -> Tiling.fiveUniformTwoOneOneOne3(8, 8),
    "5-uniform/(▲⁶; ▲⁴.⬣; ▲².⬣²; [2x ⬣³])_alt"   -> Tiling.fiveUniformTwoOneOneOne4(8, 8),
    "5-uniform/(▲⁶; ▲⁴.⬣; ▲².⬣²; [2x ⬣³])_alt2"  -> Tiling.fiveUniformTwoOneOneOne5(8, 8),
    "5-uniform/(▲⁶; ▲⁴.⬣; ▲².⬣²; [2x ⬣³])_alt3"  -> Tiling.fiveUniformTwoOneOneOne6(8, 8),
    "5-uniform/(▲⁶; [3x ▲².⬣²]; ⬣³)"             -> Tiling.fiveUniformThreeOneOne(8, 8),
    "5-uniform/(▲⁶; [3x ▲².⬣²]; ⬣³)_alt2"        -> Tiling.fiveUniformThreeOneOne2(8, 8),
    "5-uniform/([3x ▲⁶]; ▲⁴.⬣; ▲².⬣²)"           -> Tiling.fiveUniformThreeOneOne3(8, 8),
    "5-uniform/([3x ▲⁶]; ▲⁴.⬣; ▲².⬣²)_alt"       -> Tiling.fiveUniformThreeOneOne4(8, 8),
    "5-uniform/(▲⁶; [3x ⬣³]; ▲².⬣²)"             -> Tiling.fiveUniformTwoTwoOne(8, 8),
    "5-uniform/([2x ▲⁶]; ▲⁴.⬣; [2x ▲².⬣²])"      -> Tiling.fiveUniformTwoTwoOne2(8, 8),
    "5-uniform/(▲⁶; [2x ▲².⬣²]; [2x ⬣³])"        -> Tiling.fiveUniformTwoTwoOne3(8, 8),
    "5-uniform/([2x ▲⁶]; [2x ▲⁴.⬣]; ▲².⬣²)"      -> Tiling.fiveUniformTwoTwoOne4(8, 8),
    "5-uniform/(▲³.■²; [2x ▲².⬣²]; [2x ▲.■².⬣])" -> Tiling.fiveUniformTwoTwoOne5(12, 12),
    "5-uniform/([4x ▲⁶]; ▲⁴.⬣)"                  -> Tiling.fiveUniformFourOne(8, 8),
    "5-uniform/([4x ▲.⬣.▲.⬣]; ▲.■².⬣)"           -> Tiling.fiveUniformFourOne2(12, 12),
    "5-uniform/([4x ▲.⬣.▲.⬣]; ▲.■².⬣)_alt"       -> Tiling.fiveUniformFourOne3(12, 12),
    "6-uniform/(▲⁶; [4x ⬣³]; ▲².⬣²)"             -> Tiling.sixUniformFourOneOne(8, 8)
  )

  def algos(): Unit = {
    val wd = os.pwd / "out"
    os.makeDir.all(wd / "jvm" / "test" / "myAlgos" / "regular")
    os.makeDir.all(wd / "jvm" / "test" / "myAlgos" / "uniform")
    os.makeDir.all(wd / "jvm" / "test" / "myAlgos" / "2-uniform")
    os.makeDir.all(wd / "jvm" / "test" / "myAlgos" / "3-uniform")
    os.makeDir.all(wd / "jvm" / "test" / "myAlgos" / "4-uniform")
    os.makeDir.all(wd / "jvm" / "test" / "myAlgos" / "5-uniform")
    os.makeDir.all(wd / "jvm" / "test" / "myAlgos" / "6-uniform")
    m.foreach({
      case (name, t) =>
        saveFilePretty(draw(t, labelStyle = LabelStyle.NONE, markStyle = MarkStyle.GONALITY),
                       "out/jvm/test/myAlgos/" + name)
    })
  }

  def docs(): Unit = {
    val small = Tiling.fromVertex(Vertex.s("(5*2.10)"))
    val big   = Tiling.expandPattern(Full.s("(3.4.6.4)"), 100).safeGet
    saveFilePretty(draw(big, labelStyle = LabelStyle.ALL, polys = true), "docs/" + "(▲.■.⬣.■)")
    saveFilePretty(draw(small, labelStyle = LabelStyle.ALL, polys = false, perim = false), "docs/" + "(⬟².10)_label")
    saveFilePretty(draw(small, labelStyle = LabelStyle.NONE, polys = true, perim = false), "docs/" + "(⬟².10)_filled")
    saveFilePretty(draw(small, labelStyle = LabelStyle.NONE, polys = false, perim = true),
                   "docs/" + "(⬟².10)_perimeter")
    saveFilePretty(draw(Tiling.fiveUniformTwoTwoOne(8, 8),
                        labelStyle = LabelStyle.NONE,
                        markStyle = MarkStyle.GONALITY,
                        perim = false),
                   "docs/" + "(▲⁶;(⬣³)²;(▲².⬣²)²)")
  }

  def growth(): Unit = {
    val wd = os.pwd / "out"
    os.makeDir.all(wd / "jvm" / "test" / "myAlgos" / "growth")
    (Full.regularPatterns ++ Full.semiRegularPatterns).foreach(f => {
      val time = measure {
        saveFilePretty(draw(Tiling.expandPattern(f, 20).safeGet, labelStyle = LabelStyle.NONE),
                       "out/jvm/test/myAlgos/growth/" + f.toString)
      }
      println(s"$f time: $time")
    })
  }

  def scan(): Unit = {
    val wd = os.pwd / "out"
    os.makeDir.all(wd / "jvm" / "test" / "myTest" / "scan")
    val size    = 100
    val pattern = Full.s("(3.3.4.3.4)")
    val scan    = Tiling.scanPattern(pattern, size)
    scan.indices.foreach(i => {
      val time = measure {
        scan(i) match {
          case Success(f) =>
            saveFilePretty(
              draw(f, labelStyle = LabelStyle.NONE),
              "out/jvm/test/myTest/scan/" + pattern.toString + "_" + List
                .fill(size.toString.length - (i + 1).toString.length)("0")
                .mkString + (i + 1)
            )
          case Failure(_) =>
        }
      }
      println((i + 1) + s" pgon time: $time")
    })
  }

  def test(): Unit = {
    val wd = os.pwd / "out"
    os.makeDir.all(wd / "jvm" / "test" / "myTest")
    val t = Tiling.threeUniformOneOneOne8(6, 6)
    saveFilePretty(draw(t, labelStyle = LabelStyle.ALL, polys = true), "out/jvm/test/myTest/" + "test1")
  }

}
