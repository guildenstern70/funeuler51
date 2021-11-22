package net.littlelite

import org.scalatest.flatspec._
import org.scalatest.matchers._

class PrimalityTest extends AnyFlatSpec with should.Matchers {

    val primeNumbers: Array[Int] = Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37,
        41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 761, 773)

    "A Prime Number" should "be recognized as such" in {
        primeNumbers.foreach(p => Utils.isPrime(p) should be (true))
    }

    "Prime family of 13000" should "be 13001, 13003, 13007 and 13009" in {
        val indexes = Utils.combinations(n = 5)
        val result = Utils.familiesRank(13000, indexes, rank = 4)
        val expectedResult = List(List(13001,13003, 13007, 13009))
        result shouldEqual expectedResult
    }

    "Prime family of 56003" should "be 56003, 56113, 56333, 56443, 56663, 56773, 56993" in {
        val result = Main.findFamilies(56003, 2, 7)
        val expectedResult = List(56003, 56113, 56333, 56443, 56663, 56773, 56993)
        result.get shouldEqual expectedResult
    }

    "Prime family of 56003" should "rank 8 is None" in {
        val result = Main.findFamilies(56003, 2, 8)
        val expectedResult = None
        result shouldEqual expectedResult
    }

    "Prime family of 12345" should "rank 3 is None" in {
        val result = Main.findFamilies(12345, 2, 3)
        val expectedResult = None
        result shouldEqual expectedResult
    }

    "Prime family of 5 digits, 2 replacement, 7 rank" should "be 56003, 56113, 56333, 56443, 56663, 56773, 56993" in {
        val result = Main.find(5, 2, 7)
        val expectedResult = List(56003, 56113, 56333, 56443, 56663, 56773, 56993)
        result.get.number should be (56003)
        result.get.family shouldEqual expectedResult
    }

    "Prime family of 2 digits, 1 replacement, 4 rank" should "be 11 13 17 19" in {
        val result = Main.find(2, 1, 4)
        val expectedResult = List(11, 13, 17, 19)
        result.get.number should be (10)
        result.get.family shouldEqual expectedResult
    }


}
