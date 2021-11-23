package net.littlelite

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Utils {

  /**
   * Return all possible combinations of
   * replacements in a number of n digits
   *
   * @param n Digits of a number
   * @return All possible replacements
   */
  def combinations(n: Int): List[IndexedSeq[Int]] =
    List.range(1, Math.pow(2, n.toDouble).toInt)
      .map(x => _intToBinary(x, n))
      .map(_binaryToList)

  /**
   * Return only k-sized  combinations of
   * replacements in a number of n digits
   *
   * @param n Digits of a number
   * @param k Number of digits to change
   * @return All possible replacements
   */
  def kCombinations(n: Int, k: Int): List[IndexedSeq[Int]] =
    combinations(n).filter( seq => seq.size == k)

  /**
   * Given the number n, it returns a list of primes
   * with digits replaced at "indexes" places.
   *
   * @param n       Initial number
   * @param indexes List of indexes
   * @return A family of prime numbers
   */
  def primeFamily(n: Int, indexes: IndexedSeq[Int]): Seq[Int] =
    Utils
      .replacer(n, indexes)
      .filter {
        Utils.isPrime
      }

  /**
   * Return the list of families beloging to the same family having the same rank
   * @param n Starting prime number
   * @param indexesList List of digits positions to change
   * @param rank Number of items in the family
   * @return The list of families
   */
  def familiesRank(n: Int, indexesList: List[IndexedSeq[Int]], rank: Int): List[Seq[Int]] = {
    val families = new ListBuffer[Seq[Int]]()
    indexesList.foreach( i => {
      val primeFamily = Utils.primeFamily(n, i)
      if (primeFamily.nonEmpty) {
        families += primeFamily
      }
    } )
    families
      .filter( family => family.size == rank)
      .toList
  }

  /**
   * Given the number of digits, return min and max number
   * @param digits example 5
   * @return example 10000-99999
   */
  def getMinMaxFromDigits(digits: Int): (Int, Int) =
    (
      (scala.math.pow(10, digits - 1)).toInt,
      (scala.math.pow(10, digits) -1).toInt
    )


  /**
   * Return the smallest (lexicographically) sequence in the list
   * @param candidates List of list of items
   * @return Index of the lexicographically smallest list
   */
  def lexicographicalSmallest(candidates: List[Seq[Int]]): Int =
    if (candidates.size == 1)
      0
    else {
      var currentMinimum = candidates.head.head
      var minimumIndex = 0
      for ((family, index) <- candidates.view.zipWithIndex) {
        if (family.head < currentMinimum) {
          minimumIndex = index
          currentMinimum = family.head
        }
      }
      minimumIndex
    }

  /**
   * Return the smallest (lexicographically) sequence in the list of results
   * @param results List of list of items
   * @return Lexicographically smallest list
   */
    def lexicographicalSmallestResult(results: List[Result]): Int = {
      val candidates = results.map( result => result.family )
      lexicographicalSmallest(candidates)
    }


  /**
   * Given the number n, it replaces the digits at 'indexes' position
   * and returns a list of numbers with the 'index' digit replaced.
   *
   * @param n       Initial number
   * @param indexes Index of digit to be changed
   * @return A list of numbers with the 'index' digit changed
   */
  def replacer(n: Int, indexes: IndexedSeq[Int]): Seq[Int] = {

    val str = n.toString

    assume(indexes
      .map {
        _ <= str.length
      }
      .forall(_ == true)
      , "Indexes must be in range")

    List.range(0, 10).map { i =>
      _replaceChar(str, i, indexes)
    }.filter(_ >= n)

  }

  /**
   * Pure function to test primality. Uses recurtion.
   *
   * @param n Number to test if it's prime
   * @param i Seed initializer needed for recursion
   * @return True if the number is prime
   */
  def isPrime(n: Int): Boolean = {
    val end = math.sqrt(n.toDouble).toInt

    @tailrec
    def inner(d: Int): Boolean = {
      (d > end) || (n % d != 0 && n % (d + 2) != 0) && inner(d + 6)
    }

    n > 1 && ((n & 1) != 0 || n == 2) && (n % 3 != 0 || n == 3) && inner(5)
  }

  def _intToBinary(i: Int, digits: Int = 8): String =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')

  def _binaryToList(binary: String): IndexedSeq[Int] =
    0.until(binary.length).filter(binary.startsWith("1", _))

  def _replaceChar: (String, Int, IndexedSeq[Int]) => Int = (str, i, indexes) => {
    val chars = str.toCharArray
    indexes.foreach {
      chars(_) = (48 + i).toChar
    }
    String.valueOf(chars).toInt
  }
}
