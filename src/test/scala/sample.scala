
import junit.framework.TestCase
import org.junit.Assert._
import org.junit.Test
import com.knoldus.training.{Problems_21_40, Sample}

import scala.collection.mutable.ListBuffer

class ExampleSuite extends TestCase {

  var sb: StringBuilder = _
  var lb: ListBuffer[String] = _

  override def setUp() :Unit = {
    sb = new StringBuilder("ScalaTest is ")
    lb = new ListBuffer[String]
    println("Setup")
  }

  @Test
  def testInsertAt :Unit = {
    val newStr: String="new"
    val abcd: List[String]=List("a", "b", "c", "d")
    val result: List[String]=List("a", "new", "b", "c", "d")
    assertEquals(Problems_21_40.insertAt(newStr, 1, abcd), result)
  }

  @Test
  def testRange: Unit = {
    assertEquals(Problems_21_40.range(4, 9), List(4, 5, 6, 7, 8, 9))
  }

  @Test
  def testRandomSelect: Unit = {
    val l=List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    for {i <- 0 to 100} {
      assertEquals(Problems_21_40.randomSelect(3, l).length, 3)
      assertEquals(Sample.compress(Problems_21_40.randomSelect(3, l)).length, 3)
    }
  }

  @Test
  def testLotto: Unit = {
    val n = 6
    val maxInt = 49
    for {i <- 0 to 100} {
      assertEquals(Problems_21_40.lotto(n, maxInt).length, n)
      assertEquals(Sample.compress(Problems_21_40.lotto(n, maxInt)).length, n)
      assert(Problems_21_40.lotto(n, maxInt).max <= maxInt)
      assert(Problems_21_40.lotto(n, maxInt).min >= 1)
    }
  }

  @Test
  def testRandomPermutation: Unit = {
    val l=List('a', 'b', 'c', 'd', 'e', 'f')
    for {i <- 0 to 100} {
      assertEquals(Sample.compress(Problems_21_40.randomPermutation(l)).length, l.length)
      assertEquals(Problems_21_40.randomPermutation(l).sorted, l)
    }
  }

  @Test
  def testCombinations: Unit = {
    val l=List('a', 'b', 'c', 'd', 'e')
    val result=List(List('a', 'b', 'c'), List('a', 'b', 'd'), List('a', 'b', 'e'), List('a', 'c', 'd')):::
      List(List('a', 'c', 'e'), List('a', 'd', 'e'), List('b', 'c', 'd'), List('b', 'c', 'e'), List('b', 'd', 'e')):::
      List(List('c', 'd', 'e'))
    assertEquals(Problems_21_40.combinations(3, l), result)
  }

  @Test
  def testGroup: Unit = {
    val l=List('a', 'b', 'c', 'd')
    val result=Problems_21_40.group(List(1, 1, 2), l)
    result.foreach( x => assertEquals(x.flatten.sorted, l) )
  }

  @Test
  def testlsort: Unit = {
    val l=List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h)):::
      List(List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    val result=List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c)):::
      List(List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    assertEquals(Problems_21_40.lsort(l), result)
  }

  @Test
  def testlsortFreq: Unit = {
    val l=List(List('a', 'b', 'c'), List('d', 'e'), List('f', 'g', 'h')):::
      List(List('d', 'e'), List('i', 'j', 'k', 'l'), List('m', 'n'), List('o'))
    val result=List(List('i', 'j', 'k', 'l'), List('o'), List('a', 'b', 'c'), List('f', 'g', 'h'), List('d', 'e')):::
      List(List('d', 'e'), List('m', 'n'))
    assertEquals(Problems_21_40.lsortFreq(l), result)
  }

  def testIsPrime: Unit = {
    assertEquals(Problems_21_40.isPrime(2), true)
    assertEquals(Problems_21_40.isPrime(3), true)
    assertEquals(Problems_21_40.isPrime(4), false)
    assertEquals(Problems_21_40.isPrime(5), true)
    assertEquals(Problems_21_40.isPrime(6), false)
    assertEquals(Problems_21_40.isPrime(7), true)
    assertEquals(Problems_21_40.isPrime(8), false)
    assertEquals(Problems_21_40.isPrime(9), false)
    assertEquals(Problems_21_40.isPrime(10), false)
    assertEquals(Problems_21_40.isPrime(11), true)
  }

  def testGcd: Unit = {
    assertEquals(Problems_21_40.gcd(36, 63), 9)
  }

  def testIsCoprime: Unit = {
    assertEquals(Problems_21_40.isCoprime(35, 64), true)
  }

  def testPrimeFactors: Unit = {
    assertEquals(Problems_21_40.primeFactors(315), List(3, 3, 5, 7))
  }

  def testPrimeFactorMultiplicity: Unit = {
    assertEquals(Problems_21_40.primeFactorMultiplicity(315), List((3,2), (5,1), (7,1)))
  }

  def testPhi: Unit = {
    for {i <- 2 to 1000 } {
      assertEquals(Problems_21_40.phi(i), Problems_21_40.totient(i))
    }
  }

  def testListPrimesinRange: Unit = {
    val primes=List(7, 11, 13, 17, 19, 23, 29, 31)
    assertEquals(Problems_21_40.listPrimesInRange(7 to 31), primes)
  }

  def testGoldbach: Unit = {
    assertEquals(Problems_21_40.goldbach(28), (5,23))
  }

  override def tearDown(): Unit = {
    println("End")
    super.tearDown()
  }
}