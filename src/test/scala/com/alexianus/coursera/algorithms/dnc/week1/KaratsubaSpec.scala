package com.alexianus.coursera.algorithms.dnc.week1

import com.alexianus.coursera.algorithms.UnitSpec

class KaratsubaSpec extends UnitSpec {

  describe("Single Digit Addition") {
    it("should work for zeros") {
      addition("0", "0") shouldBe "0"
    }

    it("should work for ones") {
      addition("1", "1") shouldBe "2"
    }

    it("should work for same length") {
      addition("1234", "4321") shouldBe "5555"
    }

    it("should work for different length") {
      addition("1234", "43210") shouldBe "44444"
    }

    it("should handle carries") {
      addition("9999", "1") shouldBe "10000"
    }
  }

  describe("Single Digit Subtraction") {
    it("should work for zeros") {
      subtraction("0", "0") shouldBe "0"
      subtraction("000", "000") shouldBe "0"
    }

    it("should work for ones") {
      subtraction("1", "1") shouldBe "0"
    }

    it("should work for same length") {
      subtraction("4321", "1234") shouldBe "3087"
    }

    it("should work for different length") {
      subtraction("43210", "1234") shouldBe "41976"
    }

    it("should handle carries") {
      subtraction("10000", "1") shouldBe "9999"
    }
  }

  describe("Karatsuba multiplication") {
    it("should work for zeros") {
      karatsuba("0", "0") shouldBe "0"
    }

    it("should work for single digits") {
      karatsuba("3", "8") shouldBe "24"
    }

    it("should work for double digits") {
      karatsuba("30", "8") shouldBe "240"
    }

    it("should work in general") {
      karatsuba("32350", "853") shouldBe "27594550"
    }

    it("should work for the assignment question") {
      val x = "3141592653589793238462643383279502884197169399375105820974944592"
      val y = "2718281828459045235360287471352662497757247093699959574966967627"

      karatsuba(x, y) shouldBe "8539734222673567065463550869546574495034888535765114961879601127067743044893204848617875072216249073013374895871952806582723184"
    }
  }

}
