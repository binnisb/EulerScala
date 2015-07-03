/**
 * Created by brynjar on 01/07/15.
 */
class UtilsTest extends org.scalatest.FunSuite {
  test("Sixth prime is 13") {
    assert(Utils.nthPrime(6) === 13)
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
  test("Least common multiply")
  {
    assert(Utils.lcm(List(2,4,9,12)) === 36)
  }
  test("Square numbers")
  {
    assert(Utils.sqrs.take(4).toList === List[Long](1,4,9,16))
  }
  test("SumOfSquares")
  {
    assert(Utils.sumOfSquares(4) === 30)
  }
  test("SquareOfSum")
  {
    assert(Utils.squareOfSum(4) === 100)
  }
  test("atLestNPrimes")
  {
    assert(Utils.atLeastNPrimes(10).last === 31)
  }
  test("nthPrime")
  {
    assert(Utils.nthPrime(10) === 29)
  }
  test("testReadFile")
  {
    assert(Utils.readFile("src/test/resources/test.txt") === "126041981898189799870894")
  }
  test(""){
    assert(Utils.findLargestMultiply(3,Utils.readFile("src/test/resources/test.txt")) === 9*9*8)
  }
}
