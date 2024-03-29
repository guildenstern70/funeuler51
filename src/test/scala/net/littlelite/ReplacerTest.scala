package net.littlelite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ReplacerTest extends AnyFlatSpec with should.Matchers {

    "Replacement char" should "replace stars in string" in {
        val replaced = Utils._replaceChar("1**00", 3, Vector(1,2))
        replaced should be (13300)
    }

    "Replacement function" should "produce the correct prime sequence with 1-digit input" in {
        Utils.primeFamily(13, Vector(0)) should contain inOrderOnly (13, 23, 43, 53, 73, 83)
    }

    it should "produce the correct minimum with 1-digit input" in {
        val answer = Utils.primeFamily(13, Vector(0)).min
        answer should be (13)
    }

    it should "produce the correct prime sequence with 2-digits input" in {
        Utils.primeFamily(56003, Vector(2,3)) should contain inOrderOnly
                (56003, 56113, 56333, 56443, 56663, 56773, 56993)
    }

    it should "produce the correct minimum with 2-digits input" in {
        val answer = Utils.primeFamily(56003, Vector(2,3)).min
        answer should be (56003)
    }


}
