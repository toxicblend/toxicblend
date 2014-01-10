package org.toxicblend.typeconverters
import annotation.tailrec;
import scala.collection.mutable.IndexedSeq

/**
 * Some functions i found on the intenetz.
 * 
 * Made by Petr Pudl??k
 * http://stackoverflow.com/questions/8876769/best-practice-for-shifting-a-sequence-in-a-circular-manner
 */
object SeqShift {

  @tailrec
  protected 
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b);

  @inline
  protected 
  def swap[A](a: IndexedSeq[A], idx: Int, value: A): A = {
    val x = a(idx);
    a(idx) = value;
    return x;
  }

  /**
   * Time complexity: O(a.size).
   * Memory complexity: O(1).
   */
  def rotate[A](a: IndexedSeq[A], shift: Int): Unit =
    rotate(a, 0, a.size, shift);
  
  def rotate[A](a: IndexedSeq[A], start: Int, end: Int, shift: Int): Unit = {
    val len = end - start;
    if (len == 0)
      return;

    var s = shift % len;
    if (shift == 0)
      return;
    if (s < 0)
      s = len + s;

    val c = gcd(len, s);
    var i = 0;
    while (i < c) {
      var k = i;
      var x = a(start + len - s + k);
      do {
        x = swap(a, start + k, x);
        k = (k + s) % len;
      } while (k != i);
      i = i + 1;
    }
    return;
  }
}

