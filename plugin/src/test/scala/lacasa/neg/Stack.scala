package lacasa.neg

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import lacasa.util._

@RunWith(classOf[JUnit4])
class StackSpec {
  @Test
  def test1() {
    println(s"StackSpec.test1")
    expectError("confined") {
      """
        class D { }
        class C {
          import scala.spores._
          import lacasa.Box
          var b: Box[D] = _
          def m(): Unit = {
            Box.mkBox[D] { packed =>
              b = packed.box // assign box to field
            }
          }
        }
      """
    }
  }

  @Test
  def test2() {
    println(s"StackSpec.test2")
    expectError("confined") {
      """
        class D { }
        class C {
          import scala.spores._
          import lacasa.Box
          var b: lacasa.CanAccess = _
          def m(): Unit = {
            Box.mkBox[D] { packed =>
              b = packed.access // assign permission to field
            }
          }
        }
      """
    }
  }

  @Test
  def test3() {
    println(s"StackSpec.test3")
    expectError("confined") {
      """
        class D { }
        class C {
          import scala.spores._
          import lacasa.Box
          var b: Any = _
          def m(): Unit = {
            Box.mkBox[D] { packed =>
              b = packed.access // assign permission to field
            }
          }
        }
      """
    }
  }

  @Test
  def test4() {
    println(s"StackSpec.test4")
    expectError("confined") {
      """
        class E(x: Any) {}
        class D { }
        class C {
          import scala.spores._
          import lacasa.Box
          def m(): Unit = {
            Box.mkBox[D] { packed =>
              new E(packed.box)
            }
          }
        }
      """
    }
  }
}
