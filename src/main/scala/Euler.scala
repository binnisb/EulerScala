/**
 * Created by brynjar on 01/07/15.
 */

object Euler extends App {
  def Euler1(belowNum: Long = 1000): Long = Stream.iterate(1L)(_+1L) takeWhile {i=> i < belowNum} filter {i=>i%3==0 || i%5==0} sum
  def Euler2(belowNum: Long = 4000000): Long = Utils.fibs takeWhile {i => i < belowNum} filter {i=> i % 2 == 0} sum
  def Euler3(num: Long = 600851475143L): Long = Utils.factorize(num).last
  def Euler4(digits: Int = 3): Int = Utils.largestPalindrome(digits)
  def Euler5(maxDivNum: Int = 20): Int = Utils.lcm((1 to maxDivNum).toList)
  def Euler6(maxNum: Int = 100): Long = Utils.squareOfSum(maxNum) - Utils.sumOfSquares(maxNum)
  def Euler7(nthPrime: Int = 10001): Long = Utils.nthPrime(nthPrime)
}