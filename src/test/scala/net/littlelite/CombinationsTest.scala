package net.littlelite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CombinationsTest extends AnyFlatSpec with should.Matchers {

  "IntToBinary" should "produce 01111011" in {
    val intToBinary = Utils._intToBinary(123)
    intToBinary should be ("01111011")
  }

  "BinaryToList" should "produce 1, 2, 3, 4, 6, 7" in {
    val binaryToList = Utils._binaryToList("01111011")
    val sequence = IndexedSeq(1, 2, 3, 4, 6, 7)
    binaryToList shouldEqual sequence
  }

  "Combinations" should "produce the right vectors" in {
    val combinations = Utils.combinations(3)
    val listSequence = List(Vector(2), Vector(1), Vector(1, 2),
                            Vector(0), Vector(0, 2), Vector(0, 1), Vector(0, 1, 2))
    combinations shouldEqual listSequence
  }

  "KCombinations" should "produce k sized vectors" in {
    val combinations = Utils.kCombinations(3, 2)
    val listSequence = List(Vector(1, 2), Vector(0, 2), Vector(0, 1))
    combinations shouldEqual listSequence
  }

  "Lexicographically smallest" should "produce correct result" in {
    val items = List(
      Vector(12000, 12001, 21000, 12344),
      Vector(8002, 8003, 8004, 8005, 8005),
      Vector(23000, 23001, 23002)
    )
    Utils.lexicographicalSmallest(items) should be (1)
  }

  "Min max" should "work with digits number" in {
    val minmax5 = Utils.getMinMaxFromDigits(5)
    val minmax6 = Utils.getMinMaxFromDigits(6)
    minmax5._1 should be (10000)
    minmax5._2 should be (99999)
    minmax6._1 should be (100000)
    minmax6._2 should be (999999)
  }

}
