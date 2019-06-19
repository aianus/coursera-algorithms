package com.alexianus.coursera.algorithms.dnc

import scala.collection.mutable.ListBuffer

package object week2 {

  // Unimodal Max
  /*
   * You are a given a unimodal array of n distinct elements, meaning that
   * its entries are in increasing order up until its maximum element, after
   * which its elements are in decreasing order. Give an algorithm to compute
   * the maximum element that runs in O(log n) time.
   */

  def isUp[A](input: IndexedSeq[A])(implicit ordering: Ordering[A]): Boolean = {
    assert(input.length == 3)

    return (ordering.lt(input(0), input(1)) && ordering.lt(input(1), input(2)))
  }

  def isDown[A](input: IndexedSeq[A])(implicit ordering: Ordering[A]): Boolean = {
    assert(input.length == 3)

    return (ordering.gt(input(0), input(1)) && ordering.gt(input(1), input(2)))
  }

  def unimodalMax[A](input: IndexedSeq[A])(implicit ordering: Ordering[A]): Option[A]  = input.length match {
    case 0 => None
    case 1 => Some(input.head)
    case 2 => Some(input.max)
    case 3 => Some(input.max)
    case _ => {
      val leftIndex = input.length / 2 - 1
      val rightIndex = leftIndex + 3
      val middleThree = input.slice(leftIndex, rightIndex)
      if (isUp(middleThree)) {
        unimodalMax(input.slice(rightIndex, input.length))
      } else if (isDown(middleThree)) {
        unimodalMax(input.slice(0, leftIndex))
      } else {
        unimodalMax(middleThree)
      }
    }
  }

  def countInversionsMergeStep[A](input1: IndexedSeq[A], input2: IndexedSeq[A])(implicit ordering: Ordering[A]): Tuple2[Long, IndexedSeq[A]] = {
    var inversions = 0
    val sorted = new ListBuffer[A]

    var i = 0
    var j = 0

    while (i < input1.size || j < input2.size) {
      if (i == input1.size) {
        sorted.append(input2(j))
        j += 1
      } else if (j == input2.size) {
        sorted.append(input1(i))
        i += 1
      } else if (ordering.lteq(input1(i), input2(j))) {
        sorted.append(input1(i))
        i += 1
      } else {
        sorted.append(input2(j))
        j += 1
        inversions += (input1.size - i)
      }
    }

    (inversions, sorted.toIndexedSeq)
  }

  def countInversions[A](input: IndexedSeq[A])(implicit ordering: Ordering[A]): Long = {

    def countInversionsInternal[A](input: IndexedSeq[A])(implicit ordering: Ordering[A]): Tuple2[Long, IndexedSeq[A]] = input.length match {
      case 0 => (0, IndexedSeq.empty[A])
      case 1 => (0, input)
      case _ => {
        val (left, right) = input.splitAt(input.length / 2)
        val leftResult = countInversionsInternal(left)
        val rightResult = countInversionsInternal(right)
        val mergeStepResult = countInversionsMergeStep(leftResult._2, rightResult._2)

        (leftResult._1 + rightResult._1 + mergeStepResult._1, mergeStepResult._2)
      }
    }

    countInversionsInternal(input)._1
  }

}
