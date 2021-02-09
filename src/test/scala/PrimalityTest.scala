package net.littlelite

import org.scalatest._
import flatspec._
import matchers._

class PrimalityTest extends AnyFlatSpec with should.Matchers {

    val primeNumbers: Array[Int] = Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37,
        41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 761, 773)

    "A Prime Number" should "be recognized as such" in {
        primeNumbers.foreach(p => Utils.isPrime(p) should be (true))
    }

}
