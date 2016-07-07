package lacasa.samples.unique

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.control.ControlThrowable

import lacasa.{System, Box, Packed, CanAccess, Actor, ActorRef}
import Box._

import scala.spores._
import SporeConv._


// this time C is ocap!
class C {
  var arr: Array[Int] = _
}

// Container is ocap!
class Container {
  var part1: Box[C] = _
  var part2: Box[C] = _
}

class ActorA(next: ActorRef[C]) extends Actor[Container] {
  def receive(msg: Box[Container])(implicit access: CanAccess { type C = msg.C }): Unit = {
    println("ActorA received container")

    // create box that can be swapped with `part1` of container
    mkBox[C] { packed =>
      implicit val acc = packed.access
      val b: packed.box.type = packed.box

      b.open(spore { obj =>
        obj.arr = Array(100, 200, 300)
      })

      msg.swap(_.part1)((cont, newBox) => cont.part1 = newBox, b)(
        spore { (packed: Packed[C]) =>
          implicit val acc = packed.access
          val part1: packed.box.type = packed.box

          part1.open { part1Obj =>
            println(part1Obj.arr.mkString(","))
            part1Obj.arr(0) = 1000
          }

          next.send(part1)
        })
    }
  }
}

class ActorB extends Actor[C] {
  def receive(msg: Box[C])(implicit access: CanAccess { type C = msg.C }): Unit = {
    println("ActorB received object with array")
    msg.open(spore { x =>
      println(x.arr.mkString(","))
    })
  }
}

object Transfer {
  def main(args: Array[String]): Unit = try {
    val sys = System()
    val b = sys.actor[ActorB, C]
    val a = sys.actor[Container](new ActorA(b))

    mkBox[Container] { packed =>
      import packed.access
      val containerBox: packed.box.type = packed.box

      mkBox[C] { packed2 =>
        import packed2.access
        val box2: packed2.box.type = packed2.box

        // initialize object in box2 with new array
        box2.open(spore { obj =>
          obj.arr = Array(1, 2, 3, 4)
        })

        // assign `box2` to `part1` of container
        containerBox.swap(_.part1)((cont, newBox) => cont.part1 = newBox, box2)(
          spore {
            implicit val localAcc = packed.access
            val localContainerBox = containerBox
            val localA = a
              (packed: Packed[C]) =>
                import packed.access
                val ignore = packed.box

                localA.send(localContainerBox)
                Thread.sleep(500)
          }
        )
      }
    }
  } catch {
    case _: ControlThrowable =>
      /* do nothing */
  }
}
