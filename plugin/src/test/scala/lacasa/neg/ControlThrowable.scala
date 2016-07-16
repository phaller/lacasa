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
}
