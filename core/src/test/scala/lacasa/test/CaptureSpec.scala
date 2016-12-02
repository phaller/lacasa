/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.test.capture

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.spores._
import scala.spores.SporeConv._

import lacasa.{Box, Packed}
import Box._


class Data {
  var name: String = _
}

class Data2 {
  var num: Int = _
  var dat: Data = _
}

@RunWith(classOf[JUnit4])
class CaptureSpec {

  @Test
  def test(): Unit = {
    try {
      mkBox[Data] { packed =>
        implicit val acc = packed.access
        val box: packed.box.type = packed.box

        box.open { _.name = "John" }

        mkBox[Data2] { packed2 =>
          implicit val acc2 = packed2.access
          val box2: packed2.box.type = packed2.box

          box2.capture(box)(_.dat = _)(spore { (packedData: Packed[Data2]) =>
            implicit val accessData = packedData.access

            packedData.box.open { d =>
              assert(d.dat.name == "John")
            }
          })
        }
      }
    } catch {
      case t: Throwable =>
    }
  }

}
