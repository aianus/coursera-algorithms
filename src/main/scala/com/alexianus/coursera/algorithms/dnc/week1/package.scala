package com.alexianus.coursera.algorithms.dnc

package object week1 {

  def addition(x: String, y: String): String = {
    var length = Math.max(x.length, y.length)

    val xReverse = x.reverse.padTo(length, '0')
    val yReverse = y.reverse.padTo(length, '0')

    val result = new StringBuilder
    var carry = 0

    xReverse.zip(yReverse).foreach { case (d1, d2) => {
      val v1 = d1.toInt - '0'
      val v2 = d2.toInt - '0'

      val nextD = (v1 + v2 + carry) % 10
      carry = (v1 + v2 + carry) / 10

      result.append((nextD + '0').toChar)
    }}

    if (carry > 0) {
      result.append((carry + '0').toChar)
    }

    result.reverse.toString
  }

  def subtraction(x: String, y: String): String = {
    assert(x.length >= y.length)

    val xReverse = x.reverse
    val yReverse = y.reverse.padTo(x.length, '0')

    var resultBuilder = new StringBuilder
    var carry = 0

    xReverse.zip(yReverse).foreach { case (d1, d2) => {
      val v1 = d1.toInt - '0'
      val v2 = d2.toInt - '0'

      val thing = (v1 - v2 - carry)
      val nextD = if (thing >= 0) thing else thing + 10
      carry = if (thing < 0) 1 else 0

      resultBuilder.append((nextD + '0').toChar)
    }}

    if (carry > 0) {
      throw new Exception("Shouldn't happen")
    }

    val result = resultBuilder.reverse.dropWhile(_ == '0').toString

    if (result.length > 0) {
      return result
    } else {
      return "0"
    }
  }

  def karatsuba(x: String, y: String): String = {
    // Pad with leading zeros so strings are the same length
    var length = Math.max(x.length, y.length)

    // Actually, pad to next power of 2
    length = Math.max(length, Math.pow(2, (Math.log(length) / Math.log(2)).ceil).toInt)

    if (length == 1) {
      return (x.toInt * y.toInt).toString
    }

    val xPad = x.reverse.padTo(length, '0').reverse
    val yPad = y.reverse.padTo(length, '0').reverse

    val (a, b) = xPad.splitAt(length / 2)
    val (c, d) = yPad.splitAt(length / 2)

    val ac = karatsuba(a, c)
    val bd = karatsuba(b, d)
    val apb = addition(a, b)
    val cpd = addition(c, d)
    val third = karatsuba(apb, cpd)

    var middle = subtraction(third, ac)
    middle = subtraction(middle, bd)

    val nterm = ac + "0" * length
    val n2term = middle + "0" * (length / 2)

    val result = addition(
      nterm,
      addition(
        n2term,
        bd
      )
    ).dropWhile(_ == '0')

    if (result.length > 0) {
      return result
    } else {
      return "0"
    }
  }

}
