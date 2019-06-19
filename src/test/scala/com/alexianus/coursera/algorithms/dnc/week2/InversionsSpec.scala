package com.alexianus.coursera.algorithms.dnc.week2

import com.alexianus.coursera.algorithms.UnitSpec
import scala.io.Source

class InversionsSpec extends UnitSpec {

  describe("Reading input file from test resource") {
    it ("Should read the input file correctly") {
      val inputSource = Source.fromURL(getClass.getResource("/com/alexianus/coursera/algorithms/dnc/week2/assignment.input.txt"))
      val inputArray = inputSource.getLines().map(_.toInt).toArray
      inputArray.length shouldBe 100000
      inputArray(2) shouldBe 79294
    }
  }

  describe("Counting Inversions") {
    it ("should work for an empty list") {
      countInversions(Vector.empty[Integer]) shouldBe 0
    }

    it ("should work for a list of 1") {
      countInversions(Vector(1)) shouldBe 0
    }

    it ("should work for a sorted list") {
      countInversions(Vector(1, 2, 3, 4, 5, 6, 7, 8)) shouldBe 0
    }

    it ("should work for a list of 2") {
      countInversions(Vector(2, 1)) shouldBe 1
    }

    it ("should work for a list of 3") {
      countInversions(Vector(2, 1, 3)) shouldBe 1
    }

    it ("should work for a list of 3 (2)") {
      countInversions(Vector(3, 2, 1)) shouldBe 3
    }

    it ("should work for graded input") {
      val inputSource = Source.fromURL(getClass.getResource("/com/alexianus/coursera/algorithms/dnc/week2/assignment.input.txt"))
      val inputArray = inputSource.getLines().map(_.toInt).toArray

      countInversions(inputArray) shouldBe 2407905288L
    }
  }

}
