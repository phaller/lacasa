/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.test

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.{Future, Promise, Await}
import scala.concurrent.duration._

import lacasa.{System, Box, CanAccess, Actor, ActorRef}
import Box._


class NonSneaky {
  def process(a: Array[Int]): Unit = {
    for (i <- 0 until a.length)
      a(i) = a(i) + 1
  }
}

class ActorA(next: ActorRef[C]) extends Actor[C] {
  def receive(msg: Box[C])(implicit access: CanAccess { type C = msg.C }): Unit = {
    msg.open({ (obj: C) =>
      // OK: update array
      obj.arr(0) = 100

      // OK: create instance of ocap class
      val ns = new NonSneaky
      ns.process(obj.arr)
    })
    next.send(msg)({ () => })
  }
}

class ActorB(p: Promise[String]) extends Actor[C] {
  def receive(msg: Box[C])(implicit access: CanAccess { type C = msg.C }): Unit = {
    msg.open({ x =>
      p.success(x.arr.mkString(","))
    })
  }
}

class C {
  var arr: Array[Int] = _
}


@RunWith(classOf[JUnit4])
class Spec {

  @Test
  def test(): Unit = {
    // to check result
    val p: Promise[String] = Promise()

    val sys = System()
    val b = sys.actor[C](new ActorB(p))
    val a = sys.actor[C](new ActorA(b))

    try {
      mkBox[C] { packed =>
        import packed.access
        val box: packed.box.type = packed.box

        // initialize object in box with new array
        box.open({ obj =>
          obj.arr = Array(1, 2, 3, 4)
        })

        a.send(box)({ () => })
      }
    } catch {
      case t: Throwable =>
        val res = Await.result(p.future, 2.seconds)
        assert(res == "101,3,4,5")
    }

  }

}
