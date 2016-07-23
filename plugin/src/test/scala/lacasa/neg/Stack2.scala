/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.neg

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import lacasa.util._

@RunWith(classOf[JUnit4])
class Stack2Spec {
  @Test
  def test1() {
    println(s"Stack2Spec.test1")
    expectError("confined") {
      """
        class D { }
        class C {
          import scala.spores._
          import lacasa.Box
          def m(): Unit = {
            Box.mkBox[D] { packed =>
              val fun = () => {
                val acc = packed.access
              }
            }
          }
        }
      """
    }
  }

  @Test
  def test2() {
    println(s"Stack2Spec.test2")
    expectError("propagated") {
      """
        class D { var arr: Array[Int] = _ }
        class C {
          import scala.spores._
          import lacasa.Box
          def m(): Unit = {
            try {
              Box.mkBox[D] { packed =>
                val access = packed.access
              }
            } catch {
              case ct: scala.util.control.ControlThrowable =>
            }
          }
        }
      """
    }
  }

}
