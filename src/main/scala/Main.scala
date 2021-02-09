package net.littlelite

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
object Main
{
    final val VERSION = "v.0.1.0"

    def main(args: Array[String]): Unit = {
        println(s"Euler51 $VERSION")
        familyExplorer(Utils.combinations(5), 10000, 158000)
    }

    def familyExplorer(combinations: List[IndexedSeq[Int]], min: Int, max: Int): Unit = {
        var currentMaxRank = 0
        println("\n  Family Rank        |  Minimum ")
        println("=========================================")
        combinations.foreach { combo =>
            currentMaxRank = fixedVectorExplorer(currentMaxRank, min, max, combo)
        }
    }

    def fixedVectorExplorer(currentMaxRank:Int,  min: Int, max: Int, combo: IndexedSeq[Int]): Int = {
        var curMax = currentMaxRank
        List.range(min, max).foreach { number =>
            val family = Utils.primeFamily(number, combo)
            val rank = family.length
            if (rank > curMax) {
                curMax = rank
                println(s"  $rank                  |  ${family.min}")
            }
        }
        curMax
    }
}
