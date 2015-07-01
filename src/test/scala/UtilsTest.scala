/**
 * Created by brynjar on 01/07/15.
 */
class UtilsTest extends org.scalatest.FunSuite {
  test("numsFrom generates natural numbers") {
    assert(Utils.numsFrom(0).take(5).toList.last === 4L)
  }
  test("Fifth sieve prime is 11") {
    assert(Utils.sieve(Utils.numsFrom(2)).take(5).toList.last === 11)
  }
  test("Sixth prime is 13") {
    assert(Utils.primes.take(6).toList.last === 13)
  }
}
