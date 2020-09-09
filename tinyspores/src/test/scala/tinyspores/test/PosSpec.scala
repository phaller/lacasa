/**
 * Copyright (C) 2016 Philipp Haller
 */
package tinyspores
package test

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import Spore._


trait Safe[T]

object Safe {
  implicit val intIsSafe = new Safe[Int] {}
}

class Box[T](instance: T) {
  def open(s: Spore[T, Unit] { type CC = Safe[_] }): Unit =
    s(instance)
}

@RunWith(classOf[JUnit4])
class PosSpec {

  @Test
  def test(): Unit = {
    val box = new Box(5)
    val y = 15
    box.open { (x: Int) =>
      val res = x + y  // capture `y`
      assert(res == 20)
    }
  }

}
