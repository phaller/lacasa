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
}
