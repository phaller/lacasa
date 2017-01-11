package lacasa.akka

import akka.actor.{Actor, Props, ActorSystem, ActorRef}
import akka.event.{Logging, LoggingAdapter}

import scala.spores._

import lacasa.{Box, CanAccess, Safe, Packed, NoReturnControl}
import Box.mkBox


case class Message(var s: String) {
  def this() = this("")
}

class MyActor extends SafeActor[Message] {
  val log = Logging(context.system, this)

  def receive(msg: Box[Message])(implicit acc: CanAccess { type C = msg.C }): Unit = {
    msg.open(spore { (m: Message) =>
      m match {
        case Message("test") => log.info("received test")
        case _ => log.info("received unknown message")
      }
    })
  }
}


object TestAkka {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("test-system")
    val a = SafeActorRef[Message](system.actorOf(Props[MyActor]))

    try {

      mkBox[Message] { packed =>
        implicit val access = packed.access
        val box: packed.box.type = packed.box
        box.open(spore { obj =>
          obj.s = "test"
        })
        a ! box
      }

    } catch {
      case _: NoReturnControl =>
        Box.uncheckedCatchControl
        Thread.sleep(2000)
        system.terminate()
    }

  }
}
