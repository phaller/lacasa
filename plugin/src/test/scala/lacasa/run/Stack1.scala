package lacasa.run

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.util.control.ControlThrowable

class Message {
  var arr: Array[Int] = _
}

@RunWith(classOf[JUnit4])
class Stack1Spec {

  import lacasa.Box._

  @Test
  def test1(): Unit = {
    println(s"run.Stack1Spec.test1")
    try {
      mkBox[Message] { packed =>
        implicit val access = packed.access
        packed.box open { msg =>
          msg.arr = Array(1, 2, 3, 4)
        }
      }
    } catch {
      case ct: ControlThrowable =>
        uncheckedCatchControl
        assert(true, "this should not fail!")
    }
  }

}
