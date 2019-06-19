package com.alexianus.coursera.algorithms.dnc.week2

import com.alexianus.coursera.algorithms.UnitSpec

class UnimodalSpec extends UnitSpec {

  describe("Unimodal thing") {
    it("should work for empty list") {
      unimodalMax(Vector.empty[Int]) shouldBe None
    }

    it("should work for list of one") {
      unimodalMax(Vector(7)) shouldBe Some(7)
    }

    it("should work for list of two") {
      unimodalMax(Vector(7, 8)) shouldBe Some(8)
    }

    it("should work for list of three with no down") {
      unimodalMax(Vector(7, 8, 9)) shouldBe Some(9)
    }

    it("should work for list of three with down") {
      unimodalMax(Vector(7, 6, 5)) shouldBe Some(7)
    }

    it("should work for a big list no down") {
      unimodalMax(Vector(5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) shouldBe Some(14)
    }

    it("should work for a big list no up") {
      unimodalMax(Vector(14, 13, 12, 11, 10, 9, 8, 7, 6, 5)) shouldBe Some(14)
    }

    it("should work for a big list up down") {
      unimodalMax(Vector(5, 6, 7, 8, 9, 10, 11, 10, 7, 4, 3, 1)) shouldBe Some(11)
    }
  }

}
