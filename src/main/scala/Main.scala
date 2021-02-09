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
        val results =
            Utils
                .replacer(56003, List(2,3))
                .filter { Utils.isPrime(_) }
        println(s"Results: $results")
    }
}
