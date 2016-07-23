/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.neg

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import lacasa.util._

@RunWith(classOf[JUnit4])
class BoxOcapSpec {

  @Test
  def test1() {
    println("neg.BoxOcapSpec.test1")
    expectError("NonOcap") {
      """
        object Global {
          var state = "a"
        }
        class NonOcap {
          def doIt(): Unit = {
            Global.state = "b"
          }
        }
        class Data {
          var arr: Array[Int] = _
        }
        class Test {
          import lacasa.Box._
          import scala.spores._
          def m(): Unit = {
            mkBox[Data] { packed => // ok, Data ocap
              implicit val acc = packed.access
              packed.box.open(spore {
                (d: Data) =>
                  d.arr = Array(0, 1, 2) // ok
                  val obj = new NonOcap  // not ok: cannot inst. non-ocap class
              })
            }
          }
        }
      """
    }
  }

}
