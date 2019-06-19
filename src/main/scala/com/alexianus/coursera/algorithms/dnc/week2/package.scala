package com.alexianus.coursera.algorithms.dnc

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

}
