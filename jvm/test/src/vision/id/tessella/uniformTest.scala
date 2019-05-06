package vision.id.tessella

import org.scalatest.FlatSpec

import scalax.collection.Graph
import scalax.collection.GraphPredef._ // shortcuts

import vision.id.tessella.Others.Mono
import vision.id.tessella.Tessella.Tiling

class uniformTest extends FlatSpec with ConstraintUtils with Loggable {

  setLogLevel("WARN")

  "An hexagon net" must "have gonality an uniformity equal to 1" in {
    val hexNet5: Tiling = Tiling.hexagonNet(5, 5)
    assert(hexNet5.gonality === 1)
    assert(hexNet5.uniformity() === 1)
  }

  "A square net" must "have uniformity 1" in {
    assert(Tiling.squareNet(5, 5).uniformity() === 1)
  }

  "A triangle net" must "have uniformity 1" in {
    assert(Tiling.triangleNet(6, 6).uniformity() === 1)
  }

  "A tessellation of (3*2.4.3.4)" must "have uniformity 1" in {
    assert(Mono.expandPattern(Full.s("(3*2.4.3.4)"), 35).safeGet.uniformity() === 1)
  }

  val fourUniform: Tiling = Tiling.fourUniformTwoOneOne2(6, 6)

  "An 6x6 hexagon net variant" can "have gonality 3 and uniformity 4" in {
    assert(fourUniform.gonality === 3)
    assert(fourUniform.uniformity() === 4)
  }

  it must "have 4 classes of symmetry" in {
    assert(
      fourUniform.mapUniforms() === Map(
        Full.s("(3*6)")     -> List(List(98)),
        Full.s("(3*2.6*2)") -> List(List(46, 33, 65, 34, 35, 48, 47)),
        Full.s("(6*3)")     -> List(List(37, 61, 63, 50, 51, 62), List(60, 32, 64, 49, 36))
      ))
  }

//  val fiveUniform: Tiling = Tiling.fiveUniformTwoTwoOne(9, 6)
//
//  "Another 9x6 hexagon net variant" can "have gonality 3 and uniformity 5" in {
//    assert(fiveUniform.gonality === 3)
//    assert(fiveUniform.uniformity() === 5)
//  }
//
//  it must "have 5 classes of symmetry" in {
//    assert(fiveUniform.mapUniforms() === Map(
//      Full.s("(3*6)") -> List(
//        List(141, 144)),
//      Full.s("(3*2.6*2)") -> List(
//        List(85, 92, 73, 54, 66, 47),
//        List(46, 93, 84, 74, 65, 53, 86, 91, 48, 67, 72, 55)),
//      Full.s("(6*3)") -> List(
//        List(69, 88, 89, 70, 44, 95, 50, 51),
//        List(52, 45, 64, 71, 49, 87, 75, 94, 68, 90))
//    ))
//  }
//
//  val sixUniform: Tiling = Tiling.sixUniformFourOneOne(9, 9)
//
//  "A third 9x9 hexagon net variant" can "have gonality 3 and uniformity 6" in {
//    assert(sixUniform.gonality === 3)
//    assert(sixUniform.uniformity() === 6)
//  }
//
//  it must "have 6 classes of symmetry" in {
//    assert(sixUniform.mapUniforms() === Map(
//      Full.s("(3*6)") -> List(
//        List(202, 201, 204)),
//      Full.s("(3*2.6*2)") -> List(
//        List(69, 153, 106, 89, 74, 85, 70, 53, 73, 105, 54, 86, 154, 90),
//        List(88, 71, 155, 72, 87, 104, 55)),
//      Full.s("(6*3)") -> List(
//        List(52, 152, 84, 92, 124, 91, 108, 135, 67, 75, 151, 51, 107, 68),
//        List(115, 46, 147, 132, 129, 128, 148, 45, 64, 44, 144, 113, 130, 112, 145, 48, 95, 127, 114, 146, 131, 47, 111),
//        List(110, 125, 93, 133, 65, 109, 134, 149, 49, 66, 150, 50, 126, 94))
//    ))
//  }

  "A fourth 6x6 hexagon net variant" can "have gonality 3 and uniformity 3" in {
    val threeUniform: Tiling = Tiling.threeUniformOneOneOne(6, 6)
    assert(threeUniform.gonality === 3)
    assert(threeUniform.uniformity() === 3)
  }

  "A fifth 6x6 hexagon net variant" can "have gonality 2 and uniformity 3" in {
    val threeUniform2: Tiling = Tiling.threeUniformTwoOne(6, 6)
    assert(threeUniform2.gonality === 2)
    assert(threeUniform2.uniformity() === 3)
  }

  "A sixth 5x5 hexagon net variant" can "have gonality 2 and uniformity 2" in {
    val twoUniform: Tiling = Tiling.twoUniform(5, 5)
    assert(twoUniform.gonality === 2)
    assert(twoUniform.uniformity() === 2)
  }

  "A seventh 5x5 hexagon net variant" can "have gonality 2 and uniformity 2" in {
    val twoUniform2: Tiling = Tiling.twoUniform2(5, 5)
    assert(twoUniform2.gonality === 2)
    assert(twoUniform2.uniformity() === 2)
  }

  "A tessellation of (3.4.3.12; 3.12*2) " must "have 2 classes of symmetry" in {
    val t = Tiling.fromG(
      Graph(
        60 ~ 61,
        60 ~ 51,
        52 ~ 53,
        31 ~ 32,
        31 ~ 52,
        2 ~ 3,
        2 ~ 23,
        54 ~ 55,
        25 ~ 26,
        4 ~ 5,
        4 ~ 24,
        48 ~ 49,
        27 ~ 28,
        19 ~ 20,
        50 ~ 60,
        50 ~ 51,
        42 ~ 43,
        42 ~ 33,
        21 ~ 22,
        21 ~ 34,
        65 ~ 66,
        44 ~ 45,
        23 ~ 25,
        23 ~ 22,
        23 ~ 24,
        15 ~ 16,
        67 ~ 68,
        46 ~ 47,
        38 ~ 39,
        17 ~ 18,
        69 ~ 11,
        69 ~ 12,
        69 ~ 70,
        61 ~ 62,
        40 ~ 26,
        40 ~ 27,
        11 ~ 12,
        63 ~ 64,
        34 ~ 35,
        13 ~ 1,
        13 ~ 14,
        5 ~ 6,
        57 ~ 58,
        36 ~ 37,
        28 ~ 29,
        7 ~ 8,
        59 ~ 61,
        59 ~ 10,
        59 ~ 60,
        51 ~ 8,
        51 ~ 9,
        30 ~ 52,
        30 ~ 31,
        9 ~ 10,
        9 ~ 59,
        1 ~ 2,
        53 ~ 54,
        32 ~ 42,
        32 ~ 33,
        24 ~ 25,
        24 ~ 3,
        3 ~ 1,
        3 ~ 4,
        55 ~ 56,
        47 ~ 48,
        26 ~ 27,
        70 ~ 15,
        70 ~ 14,
        49 ~ 50,
        20 ~ 21,
        20 ~ 34,
        43 ~ 44,
        22 ~ 2,
        14 ~ 15,
        14 ~ 12,
        66 ~ 67,
        45 ~ 46,
        37 ~ 38,
        16 ~ 17,
        68 ~ 69,
        39 ~ 40,
        18 ~ 19,
        10 ~ 11,
        62 ~ 63,
        41 ~ 7,
        41 ~ 43,
        41 ~ 42,
        33 ~ 6,
        33 ~ 5,
        12 ~ 13,
        64 ~ 65,
        56 ~ 57,
        35 ~ 36,
        6 ~ 7,
        6 ~ 41,
        58 ~ 44,
        58 ~ 45,
        29 ~ 30,
        8 ~ 9
      ))
    assert(
      t.mapUniforms() === Map(
        Full.s("(3.4.3.12)") -> List(List(42, 24, 6, 33, 41, 2, 3, 23)),
        Full.s("(3.12*2)")   -> List(List(5, 1, 7, 4))
      ))
  }

}
