/**
 * Created by brynjar on 01/07/15.
 */

object Euler extends App {
  def Euler1(belowNum: Long = 1000): Long = Stream.iterate(1L)(_ + 1L) takeWhile { i => i < belowNum } filter { i => i % 3 == 0 || i % 5 == 0 } sum

  def Euler2(belowNum: Long = 4000000): Long = Utils.fibs takeWhile { i => i < belowNum } filter { i => i % 2 == 0 } sum

  def Euler3(num: Long = 600851475143L): Long = Utils.factorize(num).last

  def Euler4(digits: Int = 3): Int = Utils.largestPalindrome(digits)

  def Euler5(maxDivNum: Int = 20): Int = Utils.lcm((1 to maxDivNum).toList)

  def Euler6(maxNum: Int = 100): Long = Utils.squareOfSum(maxNum) - Utils.sumOfSquares(maxNum)

  def Euler7(nthPrime: Int = 10001): Long = Utils.nthPrime(nthPrime)

  def Euler8(nrAdj: Int = 13, nums: String = Utils.readFile("src/main/resources/Euler8.txt")): Long = Utils.findLargestMultiply(nrAdj, nums)

  def Euler9(tripSum: Int = 1000): Long = Utils.pythagoreanTripletMul(tripSum)

  def Euler10(num: Int = 2000000): Long = Utils.primesUnder(num).foldLeft(0L)(_ + _)

  def Euler11(num: Int = 4, grid: Array[Array[Int]] = Utils.readGrid("src/main/resources/Euler11.txt")): Long = Utils.findLagresgtMulGrid(num, grid)

  def Euler12(numFactorsGreaterThan: Int = 500): Long = Utils.triangleNumFactors(numFactorsGreaterThan)

  def Euler13(firstDigits: Int = 10, nums: Vector[BigInt] = Utils.readBig("src/main/resources/Euler13.txt")): String = Utils.sumBig(nums).toString().slice(0, 10)

  def Euler14(underNumber: Int = 1000000): Long = Utils.chainLength(underNumber)._1

  def Euler15(sqrs: Int = 20): Long = Utils.oneWayRoutes(sqrs)

  def Euler16(dig: Int = 2, pow: Int=1000) = Utils.sumDigits(dig,pow)

  def Euler17(toNum: Int = 1000): Int = Utils.countChars(toNum)
}