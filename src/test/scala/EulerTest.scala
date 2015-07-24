import org.scalatest.FunSuite

/**
 * Created by brynjar on 01/07/15.
 */
class EulerTest extends FunSuite {
  test("Euler1") {
    assert(Euler.Euler1(10) === 23)
  }
  test("Euler2") {
    assert(Euler.Euler2(90) === 44)
  }
  test("Euler3") {
    assert(Euler.Euler3(13195) === 29)
  }
  test("Euler4")
  {
    assert(Euler.Euler4(2) === 9009)
  }
  test("Euler5") {
    assert(Euler.Euler5(10) === 2520)
  }
  test("Euler6") {
    assert(Euler.Euler6(10) === 2640)
  }
  test("Euler7") {
    assert(Euler.Euler7(6) === 13)
  }
  test("Euler8") {
    assert(Euler.Euler8(4,Utils.readFile("src/main/resources/Euler8.txt")) === 5832)
  }
  test("Euler9") {
    assert(Euler.Euler9(12) === 3*4*5)
  }
  test("Euler10") {
    assert(Euler.Euler10(10) === 17)
  }
  test("Euler11") {
    assert(Euler.Euler11(2,Utils.readGrid("src/test/resources/grid.txt")) === 97*99)
  }
  test("Euler12") {
    assert(Euler.Euler12(5) === 28)
  }
  test("Euler13") {
    assert(Euler.Euler13(10,Utils.readBig("src/test/resources/bigInt.txt")).toString().slice(0,10) === "1231231231")
  }
  test("Euler14") {
    assert(Euler.Euler14(10) === 9)
  }
  test("Euler15") {
    assert(Euler.Euler15(3) === 20)
  }
  test("Euler16") {
    assert(Euler.Euler16(2,5) === 5)
  }
  test("Euler17") {
    assert(Euler.Euler17(5) === 19)
  }
  test("Euler18") {
    assert(Euler.Euler18(Utils.readTriangle("src/test/resources/triangle.txt")) === 23)
  }
}
