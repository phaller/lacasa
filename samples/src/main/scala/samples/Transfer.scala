package lacasa.samples

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import lacasa.{System, Box, CanAccess, Actor, ActorRef}
import Box._


class ActorA(next: ActorRef[C]) extends Actor[C] {
  def receive(msg: Box[C])(implicit access: CanAccess { type C = msg.C }): Unit = {
    println("ActorA received object with array")
    msg.open { obj =>
      // OK: update array
      obj.arr(0) = 100

      // NOT OK: leak array
      // SomeObject.fld = obj.arr
    }
    next.send(msg)
  }
}

class ActorB extends Actor[C] {
  def receive(msg: Box[C])(implicit access: CanAccess { type C = msg.C }): Unit = {
    println("ActorB received object with array")
    msg.open { x =>
      println(x.arr.mkString("[", ",", "]"))
    }
  }
}

object C {
  var fld: Array[Int] = _
}

class C {
  var arr: Array[Int] = _
  def leak(): Unit = {
    //C.fld = arr
  }
}

object Transfer {
  def main(args: Array[String]): Unit = {
    val sys = System()
    val b = sys.actor[ActorB, C]
    val a = sys.actor[C](new ActorA(b))

    // LaCasa plugin checks that `C` is an ocap class
    box[C] { packed =>
      import packed.access
      val box: packed.box.type = packed.box

      // initialize object in box with new array
      box.open { obj =>
        obj.arr = Array(1, 2, 3, 4)
      }

      a.send(box)

      Thread.sleep(500)
    }
  }
}

/*
object Transfer {
  def main(args: Array[String]): Unit = {
    val sys = System()
    val b = sys.actor[ActorB, Array[Int]]
    val a = sys.actor[Array[Int]](new ActorA(b))

    val box = Box.make(Array(1, 2, 3, 4)) // will be illegal
    implicit val access = new CanAccess { type C = box.C } // will be illegal

    a.send(box)

    Thread.sleep(500)
  }
}
*/
