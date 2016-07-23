/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.samples

import lacasa.{LaCasaApp, Box, CanAccess}
import Box._  // imports `mkBox`

import scala.spores._


class Test {
  var arr: Array[Int] = _
}

// stateful class
class HasState {
  var i1 = 5
  var i2 = 10

  def m(box1: Box[Test], box2: Box[Test])(implicit a1: CanAccess {type C = box1.C}, a2: CanAccess {type C = box2.C}): Unit = {
    // 1. mutate own state
    this.i1 = 6

    // 2. open boxes:
    // capture immutable data and put into boxes
    //
    // note that it would not be OK to capture "this";
    // however, in this case we're only capturing Ints, which is OK.
    box1.open(spore {
      val local1 = this.i1
      val local2 = this.i2
      (t: Test) =>
        t.arr = Array(local1, local2)
    })

    box2.open(spore {
      (t: Test) => t.arr = Array(0, 0, 0)
    })
  }
}

object Borrow extends LaCasaApp {

  // expected output:
  // 6,10
  // 0,0,0
  def lcMain(args: Array[String]): Unit = {
    val hs = new HasState

    mkBox[Test] { packed1 =>
      implicit val acc1 = packed1.access

      mkBox[Test] { packed2 =>
        implicit val acc2 = packed2.access

        // pass both boxes to `m`, but don't consume them
        hs.m(packed1.box, packed2.box)

        // open boxes to print their contents
        packed1.box.open(spore {
          (t: Test) => println(t.arr.mkString(","))
        })
        packed2.box.open(spore {
          (t: Test) => println(t.arr.mkString(","))
        })
      }
    }
  }
}
