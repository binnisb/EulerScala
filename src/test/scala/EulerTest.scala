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
}
