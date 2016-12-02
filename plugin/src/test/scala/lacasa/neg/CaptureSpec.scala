/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.test.plugin.capture

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import lacasa.util._


@RunWith(classOf[JUnit4])
class CaptureSpec {

  @Test
  def test() {
    println(s"CaptureSpec.test")
    expectError("invalid reference to value acc") {
      """
        import lacasa.Box
        import Box._
        import scala.spores._
        class Data {
          var name: String = _
        }
        class Data2 {
          var num: Int = _
          var dat: Data = _
        }
        object Use {
          mkBox[Data] { packed =>
            implicit val acc = packed.access
            val box: packed.box.type = packed.box

            box.open { _.name = "John" }

            mkBox[Data2] { packed2 =>
              implicit val acc2 = packed2.access
              val box2: packed2.box.type = packed2.box

              box2.capture(box)((x, y) => x.dat = y)(spore {
                val localBox = box
                (d: Data2) =>
                  localBox.open { x => assert(false) }
                  assert(d.dat.name == "John")
              })
            }
          }
        }
      """
    }
  }

}
