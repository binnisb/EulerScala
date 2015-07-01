/**
 * Created by brynjar on 01/07/15.
 */
class UtilsTest extends org.scalatest.FunSuite {
  test("Fifth sieve prime is 11") {
    assert(Utils.sieve(Stream.iterate(2L)(_+1L)).take(5).toList.last === 11)
  }
  test("Sixth prime is 13") {
    assert(Utils.primes.take(6).toList.last === 13)
  }
  test("Prime factors of 10 and 11"){
    assert(Utils.factorize(10).last === 5)
    assert(Utils.factorize(11).last === 11)
  }
  test("Largest fib under 90") {
    assert(Utils.fibs.takeWhile(i=> i < 90).last === 89)
  }
  test("Largest Palindrome")
  {
    assert(Utils.palindromes(2).dequeue === 9009)
  }
}
