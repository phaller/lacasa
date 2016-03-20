package lacasa.samples

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import lacasa.{System, Box, CanAccess, Actor, ActorRef}
import Box._

import scala.spores._


object SomeObject {
  var fld: Array[Int] = _
}

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

class ActorA(next: ActorRef[C]) extends Actor[C] {
  def receive(msg: Box[C])(implicit access: CanAccess { type C = msg.C }): Unit = {
    println("ActorA received object with array")
    msg.open(spore { (obj: C) =>
      // OK: update array
      obj.arr(0) = 100

      // NOT OK: leak array
      //SomeObject.fld = obj.arr

      // NOT OK: create instance of non-ocap class
      //val s = new Sneaky
      //s.process(obj.arr)

      // OK: create instance of ocap class
      val ns = new NonSneaky
      ns.process(obj.arr)
    })
    next.send(msg)
  }
}

class ActorB extends Actor[C] {
  def receive(msg: Box[C])(implicit access: CanAccess { type C = msg.C }): Unit = {
    println("ActorB received object with array")
    msg.open(spore { (x: C) =>
      println(x.arr.mkString("[", ",", "]"))
    })
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
    val x = 4
    box[C] { packed =>
      import packed.access
      val box: packed.box.type = packed.box

      // initialize object in box with new array
      box.open(spore {
        //val localX = x
        (obj: C) =>
          obj.arr = Array(1, 2, 3, 4/*localX*/)
      })

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
