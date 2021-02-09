package net.littlelite

object Utils
{

    /**
     * Given the number n, it replaces the digit at 'index' position
     * and returns a list of numbers with the 'index' digit replaced.
     * @param n Initial number
     * @param index Index of digit to be changed
     * @return A list of numbers with the 'index' digit changed
     */
    def replacer(n: Int, index: Int): Seq[Int] = {

        val str = n.toString
        if (index > str.length)
            throw new RuntimeException("Index must be minor or equal to " + str.length)

        val ciphers = List.range(1, 10)
        ciphers.map{  i =>
            _replaceChar(str, i, index)
        }
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
        while (scala.math.pow(i, 2) <= n)
        {
            if (n % i == 0 || n % (i + 2) == 0)
                return false
            return isPrime(n, _inc6(i))
        }
        true
    }

    val _inc6: (Int) => Int = (n) => n + 6

    val _replaceChar: (String, Int, Int) => Int = (str, i, index) => {
        val chars = str.toCharArray
        chars(index) = (48+i).toChar
        String.valueOf(chars).toInt
    }
}

