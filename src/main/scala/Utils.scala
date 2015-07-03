import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Created by brynjar on 01/07/15.
 */
object Utils {

//  def sieve(s: Stream[Long]): Stream[Long] = s.head #:: sieve(s.tail.filter(_ % s.head != 0))

//  val primes = sieve(2L #:: Stream.iterate(3L)(_+2))

  def atLeastNPrimes(nth: Int): Vector[Int] = {
    val upperBound = if (nth > 6) (nth*math.log(nth) + nth*math.log(math.log(nth))).toInt+1 else 20
    val maxIter = math.sqrt(upperBound).toInt
    var sieve = Vector.fill[Boolean](upperBound)(true)
    sieve = sieve.updated(0,false).updated(1,false)
    (2 to maxIter)
      .map {i=>Stream.from(i+i,i)
                 .takeWhile(_<sieve.size)
                 .toList
                 .map {j=>sieve=sieve.updated(j,false)}
      }
    sieve.zipWithIndex.filter(_._1).map(_._2)
  }
  def nthPrime(nth: Int) = atLeastNPrimes(nth)(nth-1)

  def factorize(num: Long): List[Long] = {
    @tailrec
    def factors(n: Long, f: List[Long], c: Long, s: Long): List[Long] = {
      (n, f, c, s) match {
        case (1, facs, _, _) => facs
        case (num, facs, curr, _) if (num % curr == 0) => factors(num / curr, facs :+ curr, curr,
                                                                  math.sqrt(num / curr).toInt)
        case (num, facs, curr, sqrt) if (curr < sqrt) => factors(num, facs, curr + {
          if (curr != 2) 2 else 1
        }, sqrt)
        case (num, facs, curr, sqrt) if (curr >= sqrt) => factors(1, facs :+ num, curr, 0)

      }
    }
    factors(num, List[Long](), 2L, math.sqrt(num).toInt)
  }

  val fibs: Stream[Long] = 0L #:: 1L #:: (fibs zip fibs.tail).map { t => t._1 + t._2 }

  def palindromes(num: Int): mutable.PriorityQueue[Int] = {
    def isPalindrome(num: Int): Boolean = {
      val sNum = num.toString
      sNum == sNum.reverse
    }
    val pd = mutable.PriorityQueue[Int]()
    val low = math.pow(10, num - 1).toInt
    val high = math.pow(10, num).toInt - 1
    (low to high).foreach { i =>
      (i to high).foreach { j =>
        if (isPalindrome(i * j)) pd += i * j
      }
    }
    pd
  }
  def largestPalindrome(num: Int): Int = palindromes(num).dequeue()

  def lcm(nums: List[Int]): Int = {
    nums
      .map({ i => Utils.factorize(i) })
      .map(_.groupBy(l => l).map(l => (l._1, l._2.length)))
      .reduce { (m1, m2) => (m1.keySet ++ m2.keySet)
        .map { k => (k, Vector(m1.getOrElse(k, 0), m2.getOrElse(k, 0)).max) }.toMap }
      .map(t => math.pow(t._1, t._2).toInt)
      .product
  }

  val sqrs: Stream[Long] = 1L #:: 4L #:: sqrs.zip(sqrs.tail).map {t=> t._2 + (t._2 - t._1 + 2)}

  def sumOfSquares(num: Int) : Long = {
    sqrs.take(num).sum
  }
  def squareOfSum(num: Int) : Long = {
    val s = (((1+num)*num)/2).toLong
    s*s
  }

  def findLargestMultiply(nrAdj: Int, nums: String) : Long = {
    nums.sliding(nrAdj).map(_.split("").map(_.toLong).product).max
  }
  def readFile(path: String): String = {
    io.Source.fromFile(path).getLines.mkString
  }

  def pythagoreanTripletMul(tripSum: Int): Long = {
    Utils.sqrs.take(tripSum).combinations(3)
    val v = Vector(0L) ++ Utils.sqrs.take(tripSum-3).toVector

    @tailrec
    def numsSum(i:Int, j:Int, k:Int,prods: List[(Int,Int,Int)]=List[(Int,Int,Int)](),sqrs: Vector[Long] = v): List[(Int,Int,Int)] = {
      (i,j,k) match  {
        case (i,j,_) if (i >= k)   => prods
        case (i,j,k) if (j >= k)   => numsSum(i+1,i+2,j+k-i-3,prods)
        case (i,j,k)               => numsSum(i,j+1,k-1, if (v(i)+v(j) == v(k)) prods :+ (i,j,k) else prods)
      }
    }
    println(numsSum(1,2,tripSum-3).map(t=>t._1.toLong*t._2.toLong*t._3.toLong).max)

    1L
  }
}

