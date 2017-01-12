/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
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

  @Test
  def testSwap() {
    println(s"run.Stack1Spec.testSwap")
    class C {
      var arr: Array[Int] = _
    }
    class Container {
      import lacasa.Box
      var part1: Box[C] = _
      var part2: Box[C] = _
    }
    class D {
      import lacasa.{Box, Packed, CanAccess}
      import Box.mkBox
      var fld: Any = _
      def receive(msg: Box[Container])(implicit access: CanAccess { type C = msg.C }): Unit = {
        mkBox[C] { packed =>
          implicit val acc = packed.access
          val b: packed.box.type = packed.box
          msg.swap(_.part1)((cont, newBox) => cont.part1 = newBox, b)(
            { (packed: Packed[C]) =>
              implicit val acc = packed.access
              val part1: packed.box.type = packed.box
            })
        }
      }
    }
  }

}
