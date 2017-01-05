/**
 * Copyright (C) 2016 Philipp Haller
 */
package lacasa.test

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import lacasa.Box._


class DoesNotHaveNoArgCtor(val num: Int) {
  def incNum = new DoesNotHaveNoArgCtor(num + 1)
}

@RunWith(classOf[JUnit4])
class BoxSpec {

  @Test
  def testMkBoxFor1(): Unit = {
    try {
      mkBoxFor(new DoesNotHaveNoArgCtor(0)) { packed =>
        implicit val access = packed.access
        val box: packed.box.type = packed.box
        box.open { dnh =>
          assert(dnh.num == 0)
        }
      }
    } catch {
      case t: Throwable =>
        /* do nothing */
    }
  }

  @Test
  def testMkBoxFor2(): Unit = {
    try {
      val init = new DoesNotHaveNoArgCtor(0)
      mkBoxFor(init.incNum) { packed =>
        implicit val access = packed.access
        val box: packed.box.type = packed.box
        box.open { dnh =>
          assert(dnh.num == 1)
        }
      }
    } catch {
      case t: Throwable =>
        /* do nothing */
    }
  }

}
