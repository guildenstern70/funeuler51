package net.littlelite

import scala.collection.mutable.ListBuffer

/**
 * Euler 51.
 *
 * By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible
 * values: 13, 23, 43, 53, 73, and 83, are all prime.
 * By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the
 * first example having seven primes among the ten generated numbers, yielding the
 * family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
 * Consequently 56003, being the first member of this family, is the smallest prime with this property.
 *
 * Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits)
 * with the same digit, is part of an eight prime value family.
 */
object Main {

  final val VERSION = "v.0.1.2"
  final val DIGITS = 5

  def main(args: Array[String]): Unit = {
    println(s"Euler51 $VERSION")
    val t1 = System.nanoTime
    val minmax = Utils.getMinMaxFromDigits(DIGITS)
    familyExplorer(Utils.combinations(DIGITS), minmax._1, minmax._2)
    val duration = (System.nanoTime - t1) / 1e9d
    println("Elapsed " + duration)
  }

  def find(numberOfDigits: Int, replacingNumberOfDigits: Int, rank: Int): Option[Result] = {
    val indexes: List[IndexedSeq[Int]] = Utils.kCombinations(numberOfDigits, replacingNumberOfDigits)
    val minmax = Utils.getMinMaxFromDigits(numberOfDigits)
    val resultsList = findFamiliesInRange(minmax, indexes, rank)
    if (resultsList.nonEmpty) {
      val resultIndex = Utils.lexicographicalSmallestResult(resultsList)
      Some(resultsList(resultIndex))
    } else {
      None
    }
  }

  def findFamiliesInRange(range: (Int, Int), indexes: List[IndexedSeq[Int]], rank: Int): List[Result] = {
    val resultsList = new ListBuffer[Result]()
    List.range(range._1, range._2).foreach { number =>
      val candidates = Utils.familiesRank(number, indexes, rank)
      if (candidates.nonEmpty) {
        val familyIndex = Utils.lexicographicalSmallest(candidates)
        resultsList += Result(number, rank, candidates(familyIndex))
      }
    }
    resultsList.toList
  }

  def findFamilies(number: Int, replacingNumberOfDigits: Int, rank: Int):
            Option[Seq[Int]] = {
    val indexes = Utils.kCombinations(number.toString.length, replacingNumberOfDigits)
    val candidates = Utils.familiesRank(number, indexes, rank)
    if (candidates.isEmpty)
      None
    else
      Some(candidates(Utils.lexicographicalSmallest(candidates)))
  }

  def familyExplorer(combinations: List[IndexedSeq[Int]], min: Int, max: Int): Unit = {
    var currentMaxRank = 0
    println("\n  Family Rank (L)    |  Primes ")
    println("=========================================")
    combinations.foreach { combo =>
      currentMaxRank = fixedVectorExplorer(currentMaxRank, min, max, combo)
    }
  }

  def fixedVectorExplorer(currentMaxRank: Int, min: Int, max: Int, combo: IndexedSeq[Int]): Int = {
    var curMax = currentMaxRank
    List.range(min, max).foreach { number =>
      val family = Utils.primeFamily(number, combo)
      val rank = family.length
      if (rank > curMax) {
        curMax = rank
        println(s"  $rank  ($number)           |  $family")
      }
    }
    curMax
  }
}
