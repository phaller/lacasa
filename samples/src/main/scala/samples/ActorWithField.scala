package lacasa.samples.actorwithfield

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.control.ControlThrowable

import lacasa.{System, Box, CanAccess, Actor, ActorRef, doNothing, Packed}
import Box._


class Start {
  var next: ActorRef[Message] = _
}

class Message {
  var l: List[Int] = List()
}


class ActorA extends Actor[Start] {

  override def receive(box: Box[Start])(implicit acc: CanAccess { type C = box.C }) {
    box open { s =>
      mkBox[Message] { packed =>
        implicit val access = packed.access

        packed.box open { msg =>
          msg.l = List(2, 4, 6)
        }

        s.next.send(packed.box) {
          doNothing.consume(packed.box)
        }
      }
    }
  }

}

class Data {
  var l: List[Int] = List(1)
  var tmp: Message = _
}

class ActorB extends Actor[Message] {

  var data: Box[Data] = _

  override def receive(box: Box[Message])(implicit acc: CanAccess { type C = box.C }) {
    mkBox[Data] { packed =>
      implicit val access = packed.access

      // capture received message
      packed.box.capture(box)(_.tmp = _)({ packedData =>
        implicit val accessData = packedData.access

        packedData.box.open { d =>
          d.l = d.l ::: d.tmp.l
          d.tmp = null
          println(s"list inside: ${d.l}")
        }

        swap(this.data)(x => this.data = x, packedData.box)({ (packedOld: Packed[Data]) =>
          // packedOld is uninteresting (it's null)
        })
      })

    }
  }

}

object ActorWithField {

  def main(args: Array[String]): Unit = try {
    val sys = System()
    val a = sys.actor[ActorA, Start]

    mkBox[Start] { packed =>
      implicit val acc = packed.access
      val box: packed.box.type = packed.box

      box.open({ obj =>
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
