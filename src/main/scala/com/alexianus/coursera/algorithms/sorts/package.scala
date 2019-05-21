package com.alexianus.coursera.algorithms

import scala.collection.mutable.ListBuffer

package object sorts {

  def mergeSort[A](input: Seq[A])(implicit ordering: Ordering[A]): Seq[A] = {

    def merge(x: Seq[A], y: Seq[A]): Seq[A] = {
      val (xIterator, yIterator) = (x.iterator.buffered, y.iterator.buffered)
      val result = new ListBuffer[A]

      while (xIterator.hasNext || yIterator.hasNext) {
        if (xIterator.isEmpty) {
          result.appendAll(yIterator)
        } else if (yIterator.isEmpty) {
          result.appendAll(xIterator)
        } else if (ordering.lteq(xIterator.head, yIterator.head)) {
          result.append(xIterator.next())
        } else {
          result.append(yIterator.next())
        }
      }

      return result
    }

    val inputLength = input.length

    if (inputLength < 2) {
      return input
    }

    val (firstHalf, secondHalf) = input.splitAt(inputLength / 2)

    return merge(mergeSort(firstHalf), mergeSort(secondHalf))
  }

}
