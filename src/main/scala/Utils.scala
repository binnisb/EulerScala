import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Created by brynjar on 01/07/15.
 */
object Utils {
  def sieve(s: Stream[Long]): Stream[Long] = s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  val primes = sieve(Stream.iterate(2L)({i=>i + { if (i == 2) 1L else 2L} } ) )
  def factorize(num: Long): List[Long] = {
    @tailrec
    def factors(n: Long,f: List[Long], c: Long, s: Long): List[Long] = {
      (n,f,c,s)  match {
        case (1, facs, _, _) => facs
        case (num, facs, curr, _) if (num % curr == 0) => factors(num / curr, facs :+ curr, curr, math.sqrt(num / curr).toInt)
        case (num, facs, curr, sqrt) if (curr < sqrt) => factors(num, facs, curr + {if(curr != 2) 2 else 1}, sqrt)
        case (num, facs, curr, sqrt) if (curr >= sqrt) => factors(1,facs :+ num,curr,0)

      }
    }
    factors(num,List[Long](),2L,math.sqrt(num).toInt)
  }

  val fibs: Stream[Long] = 0L #:: 1L #:: (fibs zip fibs.tail).map {t => t._1 + t._2}

  def palindromes(num: Int): mutable.PriorityQueue[Int] = {
    def isPalindrome(num: Int): Boolean = {
      val sNum = num.toString
      sNum == sNum.reverse
    }
    val pd = mutable.PriorityQueue[Int]()
    val low = math.pow(10,num-1).toInt
    val high = math.pow(10,num).toInt -1
    (low to high).foreach { i =>
      (i to high).foreach {j =>
        if (isPalindrome(i*j)) pd += i*j
      }
    }
    pd
  }
}
