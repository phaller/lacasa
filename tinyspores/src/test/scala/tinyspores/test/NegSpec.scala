/**
 * Copyright (C) 2016 Philipp Haller
 */
package tinyspores
package test

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import util._

import Spore._


@RunWith(classOf[JUnit4])
class NegSpec {

  @Test
  def testCaptureSingle(): Unit = {
    expectError("Long") {
      """
        import tinyspores.Spore
        import tinyspores.Spore._
        trait Safe[T]
        object Safe {
          implicit val intIsSafe = new Safe[Int] {}
        }
        def open(s: Spore[Int, Unit] { type CC = Safe[_] }): Unit =
          s(5)
        val y = 15L
        open { (x: Int) =>
          val res = x + y
          println(s"res: $res")
        }
      """
    }
  }

  @Test
  def testCaptureMultiple(): Unit = {
    expectError("Long") {
      """
        import tinyspores.Spore
        import tinyspores.Spore._
        trait Safe[T]
        object Safe {
          implicit val intIsSafe = new Safe[Int] {}
        }
        def open(s: Spore[Int, Unit] { type CC = Safe[_] }): Unit =
          s(5)
        val y = 15L
        val z = 3
        open { (x: Int) =>
          val res = x + y + z
          println(s"res: $res")
        }
      """
    }
  }

  @Test
  def testExcluded(): Unit = {
    expectError("Long") {
      """
        import tinyspores.Spore
        import tinyspores.Spore._
        def open(s: Spore[Int, Unit] { type Excluded = Long }): Unit =
          s(5)
        val y = 15L
        open { (x: Int) =>
          val res = x + y
          println(s"res: $res")
        }
      """
    }
  }

}
