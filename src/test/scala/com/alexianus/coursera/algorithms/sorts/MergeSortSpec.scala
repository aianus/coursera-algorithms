package com.alexianus.coursera.algorithms.sorts

import com.alexianus.coursera.algorithms.UnitSpec

class MergeSortSpec extends UnitSpec {

  describe("Merge Sort") {
    describe("when empty") {
      it("should return empty") {
        mergeSort(Seq.empty[Int]) shouldEqual Seq.empty[Int]
      }
    }

    describe("when singleton") {
      it("should return the same sequence") {
        mergeSort(Seq(1)) shouldEqual Seq(1)
      }
    }

    describe("when random") {
      it("should return the sorted sequence") {
        mergeSort(Seq(2, 4, 1, 6, 7, 3, 5, 9, 8)) shouldEqual Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)
      }
    }

    describe("when there are duplicates") {
      it("should return the sorted sequence") {
        mergeSort(Seq(2, 4, 1, 6, 7, 3, 5, 6, 8)) shouldEqual Seq(1, 2, 3, 4, 5, 6, 6, 7, 8)
      }
    }
  }

}
