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
}
