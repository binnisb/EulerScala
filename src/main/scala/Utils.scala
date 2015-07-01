/**
 * Created by brynjar on 01/07/15.
 */
object Utils {
  def numsFrom(num: Long): Stream[Long] = num #:: numsFrom(num+1L)
  def sieve(s: Stream[Long]): Stream[Long] = s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  val primes = sieve(numsFrom(2L))
}
