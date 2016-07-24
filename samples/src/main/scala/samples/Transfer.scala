/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.samples

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.control.ControlThrowable

import lacasa.{System, Box, CanAccess, Actor, ActorRef, doNothing}
import Box._

import scala.spores._


class Sneaky { // not ocap!
  def process(a: Array[Int]): Unit = {
    SomeObject.fld = a
  }
}

class NonSneaky {
  def process(a: Array[Int]): Unit = {
    for (i <- 0 until a.length)
      a(i) = a(i) + 1
  }
}

class ActorA extends Actor[Any] {
  override def receive(box: Box[Any])
      (implicit acc: CanAccess { type C = box.C }) {
    box open {
      case s: Start =>
        mkBox[Message] { packed =>
          implicit val access = packed.access
          packed.box open { msg =>
            msg.arr = Array(1, 2, 3, 4)
          }
          s.next.send(packed.box) {
            doNothing.consume(packed.box)
          }
        }
      case other => // ...
    }
  }
}

class ActorB extends Actor[Message] {
  override def receive(box: Box[Message])
      (implicit acc: CanAccess { type C = box.C }) {
    box open { msg =>
      println(msg.arr.mkString(","))
    }
  }
}

class Message {
  var arr: Array[Int] = _
  def leak(): Unit = {
    //SomeObject.fld = arr
  }
}

object SomeObject {
  var fld: Array[Int] = _
}

class Start {
  var next: ActorRef[Message] = _
}

// expected output:
// 1,2,3,4
object Transfer {

  def main(args: Array[String]): Unit = try {
    val sys = System()
    val a = sys.actor[ActorA, Any]

    // LaCasa plugin checks that `Start` is an ocap class
    mkBox[Start] { packed =>
      import packed.access
      val box: packed.box.type = packed.box

      // initialize object in box
      box.open(spore { obj =>
        val innerSys = System()
        obj.next = innerSys.actor[ActorB, Message]
      })

      a.send(box) { doNothing.consume(box) }
    }
  } catch {
    case t: ControlThrowable =>
      uncheckedCatchControl
      Thread.sleep(1000)
  }

}
