/**
 * Created by brynjar on 01/07/15.
 */

object Euler extends App {
  def Euler1(belowNum: Long = 1000): Long = Utils.numsFrom(1) takeWhile {i=> i < belowNum} filter {i=>i%3==0 || i%5==0} sum
}