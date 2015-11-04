import java.time.{DayOfWeek, LocalDate}

import scala.annotation.tailrec
import scala.collection.{mutable, immutable}

/**
 * Created by brynjar on 01/07/15.
 */
object Utils {
  def countMonthStartsWithDay(start: LocalDate, end: LocalDate, countDay: DayOfWeek): Int = {
    Iterator.iterate(start)(_.plusMonths(1)).takeWhile(!_.isAfter(end)).filter(_.getDayOfWeek == countDay).length
  }


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
  def primesUnder(num: Int) = atLeastNPrimes((num/(math.log(num)-1)).toInt).filter(_ <= num)

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
    numsSum(1,2,tripSum-3).map(t=>t._1.toLong*t._2.toLong*t._3.toLong).max
  }
  def readGrid(path: String): Array[Array[Int]] = {
    io.Source.fromFile(path).getLines.map(_.split(" ").map(_.toInt).toArray).toArray
  }
  def findLagresgtMulGrid(num: Int, grid: Array[Array[Int]]): Long = {
    val rows = grid.length
    val cols = grid(0).length
    val r = (0 until num)

    val na = Array.ofDim[Int](rows + (num - 1)*2,cols + (num - 1)*2)
    for (i<-(0 until rows); j <- (0 until cols)) {
      na((num-1)+i)((num-1)+j) = grid(i)(j)
    }
    //val values = na.flatten
    {
      for {i <- (0 until rows)
          j <- (0 until cols)
          m = List(
            r.map {k=> na(num-1+i)(num-1+j+k)}.product,
            r.map {k=> na(num-1+i)(num-1+j-k)}.product,
            r.map {k=> na(num-1+i+k)(num-1+j)}.product,
            r.map {k=> na(num-1+i-k)(num-1+j)}.product,
            r.map {k=> na(num-1+i+k)(num-1+j-k)}.product,
            r.map {k=> na(num-1+i+k)(num-1+j+k)}.product).max
      } yield m
    }.max
  }

  def triangleNumFactors(numFactorsGreaterThan: Int): Long = {
    var num = 1
    var next = 2
    var numFacs = 1
    while (numFacs <= numFactorsGreaterThan){
      num += next
      next += 1
      numFacs=2
      for (i<-(2 to math.sqrt(num).toInt)) {
        if (num%i==0) {
          numFacs+=2
        }
      }
    }
    num
  }
  def readBig(path: String): Vector[BigInt]= {
    io.Source.fromFile(path).getLines.map {i=>BigInt(i)}.toVector
  }
  def sumBig(bigs: Vector[BigInt]): BigInt = {
    bigs.sum
  }
  def chainLength(underNum: Int): (Long,Long) = {
    val chains = mutable.HashMap[Long,Long]((1L,1L),(2L,2L))
    def calcChain(num: Long, chain: Vector[Long] = Vector[Long]()): Vector[Long] = {
      (num,chains.contains(num)) match {
        case (_,true)                => num +: chain
        case (_,false) if num%2 == 0 => calcChain((num/2),num +: chain)
        case (_,false) if num%2 == 1 => calcChain((3*num)+1,num +: chain)
      }
    }
    for (i<- (3 to underNum)) {
      val c = calcChain(i)
      c.zipWithIndex.map({case (v,i) => chains(v)=i+chains(c(0))})
    }
    chains.maxBy(_._2)
  }

  def oneWayRoutes(sqrs: Int): Long = {
    val routes = mutable.HashMap[(Int,Int),Long](((0,0),0L),((0,1),1L),((1,1),2L))

    @tailrec
    def routeCounts(inds: (Int,Int),counts: List[(Int,Int,Long)]=List[(Int,Int,Long)]()): List[(Int,Int,Long)] = {
      inds match {
        case (0,j)             => routeCounts((1,j), (0,j,1L) +: counts)
        case (i,j) if (i == j) => (i,j,counts.head._3*2) +: counts
        case (i,j)             => routeCounts((i+1,j),(i,j,routes((i,j-1)) + counts.head._3 ) +: counts )

      }
    }
    (2 to sqrs).map { i=>
        val rc = routeCounts((0,i))
        rc.map {t => routes((t._1,t._2)) = t._3
      }
    }
    routes((sqrs,sqrs))
  }
  def sumDigits(num: Int, pow: Int): Int = {
    var l = BigInt(1)
    for (i<-(1 to pow)){
      l = l* num
    }
    l.toString.map{_.asDigit}.sum
  }
  def countChars(toNum: Int): Int = {
    val nums = immutable.Map[Int,Int]((0,4),(1,3),(2,3),(3,5),(4,4),(5,4),(6,3),(7,5),(8,5),(9,4),
                                      (10,3),(11,6),(12,6),(13,8),(14,8),(15,7),(16,7),(17,9),(18,8),(19,8),
                                      (20,6),(30,6),(40,6),(50,5),(60,5),(70,7),(80,6),(90,6),
                                      (100,10),(200,10),(300,12),(400,11),(500,11),(600,10),(700,12),(800,12),(900,11),(1000,11))
    Stream.from(1).take(toNum).map {
      t => t match {
        case t if (t <= 20 ) => nums(t)
        case t if (t < 100 && t % 10 == 0) => nums(t)
        case t if (t < 100) => nums(t-t%10)+nums(t%10)
        case t if (t == 1000) => nums(t)
        case t if (t % 100 == 0) => nums(t)
        case t if (t % 10 == 0) => nums(t - t%100) + nums(t%100) + 3
        case t if (t % 100 < 20) => nums(t - t%100) + nums(t%100) + 3
        case t => nums(t - t % 100) + nums(t%100 - t % 10) + nums(t % 10) + 3
      }
    }.sum
  }

  def longestPath(triangle: Vector[Vector[Int]]): Long = {
    val hm = mutable.HashMap[(Int,Int),Long]()
    (0 until triangle.length).map {i=>hm((triangle.length-1,i)) = triangle(triangle.length-1)(i)}

    for (i <- (triangle.length-2 to 0 by -1);
         j <- (0 to i)) {
      hm.update((i,j),Vector(hm(i+1,j),hm(i+1,j+1)).max + triangle(i)(j))
    }
    hm((0,0))
  }

  def readTriangle(path: String): Vector[Vector[Int]] = {
    io.Source.fromFile(path).getLines.map(_.split(" ").map(_.toInt).toVector).toVector
  }
}
