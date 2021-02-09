package net.littlelite

object Utils
{

    /**
     * Given the number n, it replaces the digits at 'indexes' position
     * and returns a list of numbers with the 'index' digit replaced.
     * @param n Initial number
     * @param indexes Index of digit to be changed
     * @return A list of numbers with the 'index' digit changed
     */
    def replacer(n: Int, indexes: List[Int]): Seq[Int] = {

        val str = n.toString

        assume( indexes
                .map { _ <= str.length }
                .forall( _ == true)
        , "Indexes must be in range")

        List.range(0, 10).map {  i =>
            _replaceChar(str, i, indexes)
        }.filter( _ >= n )

    }

    /**
     * Pure function to test primality. Uses recurtion.
     * @param n Number to test if it's prime
     * @param i Seed initializer needed for recursion
     * @return True if the number is prime
     */
    def isPrime(n: Int, i: Int = 5): Boolean = {
        if (n <= 3)
            return n > 1
        if (n % 2 == 0 || n % 3 == 0)
            return false
        while (scala.math.pow(i, 2) <= n) {
            if (n % i == 0 || n % (i + 2) == 0)
                return false
            return isPrime(n, _inc6(i))
        }
        true
    }

    val _inc6: (Int) => Int = (n) => n + 6

    val _andReducer: (Boolean, Boolean) => Boolean = (acc, b) => (acc && b)

    val _replaceChar: (String, Int, List[Int]) => Int = (str, i, indexes) => {
        val chars = str.toCharArray
        indexes.foreach { chars(_) = (48+i).toChar }
        String.valueOf(chars).toInt
    }
}

