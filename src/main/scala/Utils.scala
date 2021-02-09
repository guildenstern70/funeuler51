package net.littlelite

object Utils
{
    def inc6(n: Int): Int = n + 6

    def replaceChar(str: String, i: Int, index: Int): Int =
    {
        val chars = str.toCharArray
        chars(index) = (48+i).toChar
        val nstr = String.valueOf(chars)
        nstr.toInt
    }

    def replacer(n: Int, index: Int): Seq[Int] = {

        val str = n.toString
        if (index > str.length)
            throw new RuntimeException("Index must be minor or equal to " + str.length)

        val ciphers = List.range(1, 10)
        ciphers.map{  i =>
            replaceChar(str, i, index)
        }
    }

    def isPrime(n: Int, i: Int = 5): Boolean = {
        if (n <= 3)
            return n > 1
        if (n % 2 == 0 || n % 3 == 0)
            return false
        while (scala.math.pow(i, 2) <= n)
        {
            if (n % i == 0 || n % (i + 2) == 0)
                return false
            return isPrime(n, inc6(i))
        }
        true
    }
}

