package net.littlelite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ReplacerTest extends AnyFlatSpec with should.Matchers {

    "Replacement function" should "produce the correct prime sequence with 1-digit input" in {
        val sequence = Utils
                .replacer(13, 0)
                .filter { Utils.isPrime(_) }
        sequence should contain inOrderOnly (13, 23, 43, 53, 73, 83)
    }
}
