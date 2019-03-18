package com.knoldus.training

import com.knoldus.common.{AppConfig, KLogger}
import com.knoldus.spark.Transformers
import org.apache.log4j.Logger
import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.{SparkConf, SparkContext}

import scala.util.Random
import scala.annotation.tailrec

object Sample {


  def main(args: Array[String]):Unit = {

    // Logging Demonstration
    val LOGGER: Logger = KLogger.getLogger(this.getClass)
    val demo = "demo"
    LOGGER.info("This is a " + demo )
    LOGGER.warn("This is warning")

  }

    def last[A](l: List[A]): A = {
      l match {
        case x :: Nil => x
        case x :: tail => last(tail)
        case Nil => throw new NoSuchElementException
      }
    }

    def penultimate[A](l: List[A]): A = {
      if (l.isEmpty) { throw new NoSuchElementException }
      else if (l.tail.isEmpty ) { throw new NoSuchElementException }
      else if (l.tail.tail.isEmpty) { l.head }
      else { penultimate(l.tail) }
    }

    def nth[A](n: Int, l: List[A]): A = {
      if (l.isEmpty) { throw new NoSuchElementException }
      else if (n==0) { l.head }
      else { nth(n-1, l.tail ) }
    }

    def length[A](l: List[A]): Int = {
      if (l.isEmpty) { 0 }
      else { 1 + length(l.tail) }
    }

    def reverse[A](l: List[A]): List[A] = {
      if (l.isEmpty) { List() }
      else { l.last::reverse(l.init) }
    }

    def isPalindrome[A](l: List[A]): Boolean = {
      l==reverse(l)
    }

    def compress[A](l: List[A]): List[A] = {
      if (l.isEmpty) { l }
      else if (l.tail.isEmpty) { l }
      else if ( l.head==l.tail.head ) { compress(l.tail) }
      else { l.head::compress(l.tail) }
    }

    def combineFirstTwo[A](l: List[List[A]]): List[List[A]] = {
      if (l.length>1) {
        (l.head:::l.tail.head)::l.tail.tail
      }
      else { l }
    }

    def pack[A](l: List[A]): List[List[A]] = {
      if (l.isEmpty) { List(l) }
      else if (l.tail.isEmpty) { List(l) }
      else if ( l.head==l.tail.head ) { val temp=List(l.head)::pack(l.tail) ; combineFirstTwo(temp) }
      else { List(l.head)::pack(l.tail) }
    }

    def encode[A](l: List[A]): List[(Int, A)] = {
      val packed=pack(l)
      packed.map( x => (x.length, x.head) )
    }

    def encodeModified[A](l: List[A]): List[Any] = {
      encode(l).map( x => if (x._1==1) { x._2 } else { x } )
    }

    def decodeOne[A](x: A, n: Int): List[A] = {
      if (n==0) { List() }
      else { x::decodeOne(x, n-1) }
    }

    def decode[A](l: List[(Int, A)]): List[A] = {
      if (l.isEmpty) { List() }
      else { decodeOne(l.head._2, l.head._1):::decode(l.tail) }
    }

    def combineFirstTwoDirect[A](l: List[(Int, A)]): List[(Int, A)] = {
      if (l.length>1) {
        (l.tail.head._1 + 1, l.tail.head._2)::l.tail.tail
      }
      else { l }
    }

    def encodeDirect[A](l: List[A]): List[(Int, A)] = {
      if (l.isEmpty) { List() }
      else if (l.tail.isEmpty) { List((1, l.head)) }
      else if ( l.head==l.tail.head ) { val temp=(1, l.head)::encodeDirect(l.tail) ; combineFirstTwoDirect(temp) }
      else { (1, l.head)::encodeDirect(l.tail) }
    }

    def duplicate[A](l: List[A]): List[A] = {
      if (l.isEmpty) { l }
      else { l.head::l.head::duplicate(l.tail) }
    }

    def duplicateN[A](n: Int, l: List[A]): List[A] = {
      if (l.isEmpty) { l }
      else { decodeOne(l.head, n):::duplicateN(n, l.tail) }
    }

    def dropModN[A](n: Int, m: Int, l: List[A]): List[A] = {
      if (l.isEmpty) { l }
      else if (m%n==0) { dropModN(n, m + 1, l.tail) }
      else { l.head::dropModN(n, m + 1, l.tail) }
    }

    def drop[A](n: Int, l: List[A]): List[A] = {
      dropModN(n, 1, l)
    }

    def split[A](n: Int, l: List[A]): (List[A], List[A]) = {
      if (n==0) { (List(), l) }
      else if (l.isEmpty) { (List(), List()) }
      else { val temp=split(n-1, l.tail); (l.head::temp._1, temp._2) }
    }

    def slice[A](start: Int, stop: Int, l: List[A]): List[A] = {
      split(stop-start, split(start, l)._2)._1
    }

    def rotate[A](n: Int, l: List[A]): List[A] = {
      if (n>=0) {
        val sliced = split(n, l)
        sliced._2:::sliced._1
      }
      else {
        val sliced=split(l.length + n, l)
        sliced._2:::sliced._1
      }
    }

    def removeAt[A](n: Int, l: List[A]): (List[A], A) = {
      if (n==0) { (l.tail, l.head) }
      else { val removed=removeAt(n-1, l.tail); (l.head::removed._1, removed._2) }
    }



}

object HelperMethods {

  @tailrec
  def rangeRecursive(result: List[Int], idx1: Int, idx2: Int): List[Int] = {
    if (idx1>idx2) { result }
    else if (idx1==idx2) { idx1::result }
    else { rangeRecursive( idx1::result, idx1 + 1, idx2) }
  }

  def getFreqs[A](l: List[List[A]]): List[(List[A], Int)] = {
    l.map( x => (x, l.count( y => Sample.length(y)==Sample.length(x)) ) )
  }

  @tailrec
  def findFactorRecursive(i: Int, n: Int): Int = {
    if (n%i==0) { i }
    else { findFactorRecursive(i + 1, n) }
  }
}

object Problems_21_40 {

  def insertAt[A](el: A, idx: Int, l: List[A]): List[A] = {
    if (idx==0) { el::l }
    else { l.head::insertAt(el, idx-1, l.tail) }
  }

  def range(idx1: Int, idx2: Int): List[Int] = {
    if (idx1>idx2) { List() }
    else if (idx1==idx2) { List(idx1) }
    else { idx1::range(idx1 + 1, idx2) }
  }

  def randomSelect[A](n: Int, l: List[A]): List[A] = {
    if (n<=0) { List() }
    else { val temp=Sample.removeAt(Random.nextInt(l.length), l); temp._2::randomSelect(n-1, temp._1) }
  }

  def lotto(n: Int, max: Int): List[Int] = {
    randomSelect(n, range(1, max))
  }

  def randomPermutation[A](l: List[A]): List[A] = {
    randomSelect(Sample.length(l), l)
  }

  def combinations[A](n: Int, l: List[A]): List[List[A]] = {
    if (n==0 || Sample.length(l)==0) { List(List()) }
    else if (n==Sample.length(l)) { List(l) }
    else {
      combinations(n-1, l.tail).map( x => l.head::x ):::combinations(n, l.tail)
    }
  }

  def groupCombinations[A](n: Int, l: List[A]): List[(List[A], List[A])] = {
    combinations(n, l).map( x => (x, l.filter( y => !x.contains(y))))
  }

  def group3[A](l: List[A]): List[List[List[A]]] = {
    group(List(2, 3, 4), l)
  }

  def group[A](groupSizes: List[Int], l: List[A]): List[List[List[A]]] = {
    if (groupSizes.isEmpty) { List(List()) }
    else {
      val temp = groupCombinations(groupSizes.head, l)
      temp.flatMap(y => group(groupSizes.tail, y._2).map(x => y._1 :: x))
    }
  }

  def insertToSort[A](x: List[A], l: List[List[A]]): List[List[A]] = {
    if (l.isEmpty) { List(x) }
    else if (Sample.length(x)<=Sample.length(l.head)) { x::l }
    else { l.head::insertToSort(x, l.tail) }
  }

  def lsort[A](l: List[List[A]]): List[List[A]] = {
    if (l.isEmpty) { l }
    else { insertToSort(l.head, lsort(l.tail)) }
  }

  def insertToSortFreq[A](x: (List[A], Int), l: List[(List[A], Int)]): List[(List[A], Int)] = {
    if (l.isEmpty) { List(x) }
    else if (x._2<=l.head._2) { x::l }
    else { l.head::insertToSortFreq(x, l.tail) }
  }

  def lsortFreqHelper[A](l: List[(List[A], Int)]): List[(List[A], Int)] = {
    if (l.isEmpty) { l }
    else { insertToSortFreq(l.head, lsortFreqHelper(l.tail)) }
  }

  def lsortFreq[A](l: List[List[A]]): List[List[A]] = {
    val lFreqs=HelperMethods.getFreqs(l)
    val sorted=lsortFreqHelper(lFreqs)
    sorted.map( x => x._1 )
  }

  def isPrime(n: Int): Boolean = {
    //for {i <- range(2, n-1)} { if (n%i==0) { return false } }
    //true
    val p=(2 until n).toList.map( x => n%x ).product
    if (p==0) { false } else { true }
  }

  def gcd(a: Int, b: Int): Int = {
    if (b==0) { a } else gcd(b, a % b)
  }

  def isCoprime(a: Int, b: Int): Boolean = {
    gcd(a, b)==1
  }

  @tailrec
  def coprimesInRangeRecursive(count: Int, a: Int, b: Int, c: Int): Int = {
    if (a>b) { count }
    else {
      val coprime=if (isCoprime(a, b)) { 1 } else { 0 }
      coprimesInRangeRecursive(count + coprime, a + 1, b, c)
    }
  }

  def coprimesInRange(a: Int, b: Int, c: Int): Int = {
    if (a>b) { 0 }
    else {
      val coprime: Int=if (isCoprime(a, b)) { 1 } else { 0 }
      coprime + coprimesInRange(a + 1, b, c)
    }
  }

  def totient(a: Int): Int = {
    //coprimesInRange(1, a, a)
    coprimesInRangeRecursive(0, 1, a, a)
  }

  def findFactor(n: Int): Int = {
    //for (i <- 2 to n ) { if (n%i==0) { return i } }
    //n
    HelperMethods.findFactorRecursive(2, n)
  }

  def primeFactors(n: Int): List[Int] = {
    val m=findFactor(n)
    if (m==n) { List(n) }
    else { m::primeFactors(n/m) }
  }

  def primeFactorMultiplicity(n: Int): List[(Int, Int)] = {
    Sample.encodeDirect(primeFactors(n)).map( x => (x._2, x._1) )
  }

  def factorsToPhi(factors: List[(Int, Int)]): Int = {
    if (factors.isEmpty) { 1 }
    else { scala.math.pow(factors.head._1, factors.head._2-1).toInt*(factors.head._1-1)*factorsToPhi(factors.tail) }
  }

  def phi(m: Int): Int = {
    val factors=primeFactorMultiplicity(m)
    factorsToPhi(factors)
  }

  def comparePhiAndTotient(m: Int): Boolean = {
    val p=phi(m)
    val t=totient(m)
    println("phi(" + m + ")= " + p + " totient(" + m + ")= " + t)
    p==t
  }

  def listPrimesInRange(r: Range): List[Int] = {
    r.filter( x => isPrime(x) ).toList
  }

  def goldbachRecursive(i: Int, n: Int): (Int, Int) = {
    if (isPrime(i) && isPrime(n-i)) {  (i, n-i) }
    else { goldbachRecursive(i + 1, n) }
  }

  def goldbach(n: Int): (Int, Int) = {
    //for (i <- 2 until n) { if (isPrime(i) && isPrime(n-i)) { return (i, n-i)} }
    //(-1, -1)
    goldbachRecursive(2, n)
  }

}
