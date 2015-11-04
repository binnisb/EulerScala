/**
 * Created by brynjar on 01/07/15.
 */

import java.time.{DayOfWeek, LocalDate}

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
  test("Largest consecutive multiply"){
    assert(Utils.findLargestMultiply(3,Utils.readFile("src/test/resources/test.txt")) === 9*9*8)
  }
  test("pythagorean triplets"){
    assert(Utils.pythagoreanTripletMul(12) === 3*4*5)
  }
  test("primesUnder"){
    assert(Utils.primesUnder(10) === Vector(2,3,5,7))
  }
  test("readGrid") {
    assert(Utils.readGrid("src/test/resources/grid.txt").flatten.size === 4*4)
  }
  test("triangleFactors") {
    assert(Utils.triangleNumFactors(5) === List(1L,2L,4L,7L,14L,28L).last)
  }
  test("readBig") {
    assert(Utils.readBig("src/test/resources/bigInt.txt")(0) === BigInt("12312312311231231231123123123112312312311231231231"))
  }
  test("sumBig") {
    assert(Utils.sumBig(Vector[BigInt](BigInt("123"),BigInt("123"))) == BigInt("246"))
  }
  test("chainLength") {
    assert(Utils.chainLength(10) === (9,20))
  }
  test("oneWayRoutes") {
    assert(Utils.oneWayRoutes(3) === 20)
  }
  test("sumDigits") {
    assert(Utils.sumDigits(2,4) === 7)
  }
  test("countChars") {
    assert(Utils.countChars(5) == 19)
  }
  test("readTriangle") {
    assert(Utils.readTriangle("src/test/resources/triangle.txt") === Vector(Vector(3),Vector(7,4),Vector(2,4,6),Vector(8,5,9,3)) )
  }
  test("longestPath") {
    assert(Utils.longestPath(Utils.readTriangle("src/test/resources/triangle.txt")) === 23)
  }
  test("countDays") {
    assert(Utils.countMonthStartsWithDay(LocalDate.of(2015,10,1), LocalDate.of(2015,11,30), DayOfWeek.SUNDAY) === 1)
  }
}