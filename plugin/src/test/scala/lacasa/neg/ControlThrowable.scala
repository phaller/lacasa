/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.neg

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import lacasa.util._

@RunWith(classOf[JUnit4])
class ControlThrowableSpec {
  @Test
  def test1() {
    println(s"ControlThrowableSpec.test1")
    expectError("propagated") {
      """
        class C {
          import scala.util.control.ControlThrowable
          import lacasa.Box._
          def m(): Unit = {
            try {
              val x = 0
              val y = x + 10
              println(s"res: ${x + y}")
            } catch {
              case t: ControlThrowable =>
                println("hello")
                uncheckedCatchControl
            }
          }
        }
      """
    }
  }

  @Test
  def test2() {
    println(s"ControlThrowableSpec.test2")
    expectError("propagated") {
      """
        class C {
          import scala.util.control.ControlThrowable
          def m(): Unit = {
            try {
              throw new ControlThrowable {}
            } catch {
              case t: Throwable =>
                println("hello")
            }
          }
        }
      """
    }
  }

  @Test
  def test3() {
    println(s"ControlThrowableSpec.test3")
    expectError("propagated") {
      """
        class SpecialException(msg: String) extends RuntimeException
        class C {
          import scala.util.control.ControlThrowable
          def m(): Unit = {
            val res = try { 5 } catch {
              case s: SpecialException => println("a")
              case c: ControlThrowable => println("b")
              case t: Throwable => println("c")
            }
          }
        }
      """
    }
  }
}
