package lacasa.test.uniqueness2

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.{Future, Promise, Await}
import scala.concurrent.duration._

import scala.spores._

import lacasa.{System, Box, CanAccess, Actor, ActorRef}
import Box._


sealed abstract class Msg

final class Start extends Msg

final class Ping extends Msg {
  var count = 0
  var sender: ActorRef[Msg] = _
}

final class Pong extends Msg {
  var count = 0
}

class ActorA(next: ActorRef[Msg]) extends Actor[Msg] {
  def receive(msg: Box[Msg])(implicit access: CanAccess { type C = msg.C }): Unit = {
    msg.open(spore {
      val selfRef = this.self()
      val nextRef = next
      m => m match {
        case s: Start =>
          mkBox[Ping] { packed =>
            implicit val acc = packed.access
            val box: packed.box.type = packed.box

            // initialize `Ping` object in box
            box.open(spore {
              val localSelf = selfRef  // captures `selfRef` within `open`
              ping => ping.sender = localSelf
            })

            nextRef.send(box)
          }

        case pong: Pong =>
          if (pong.count == 100) {
            exit()
          } else {
            mkBox[Ping] { packed =>
              implicit val acc = packed.access
              val box: packed.box.type = packed.box

              // initialize `Ping` object in box with updated counter
              box.open(spore {
                val localSelf  = selfRef    // captures `selfRef` within `open`
                val localCount = pong.count // captures `pong.count` within `open`
                ping =>
                  ping.sender = localSelf
                  ping.count  = localCount + 1
              })

              nextRef.send(box)
            }
          }
      }
    })
  }
}

class ActorB(p: Promise[Boolean]) extends Actor[Msg] {
  def receive(msg: Box[Msg])(implicit access: CanAccess { type C = msg.C }): Unit = {
    msg.open(spore { x =>
      x match {
        case ping: Ping =>
          if (ping.count == 100) { // done
            p.success(true)
          } else {
            mkBox[Pong] { packed =>
              implicit val acc = packed.access
              val box: packed.box.type = packed.box

              // initialize `Pong` object in box with updated counter
              box.open(spore {
                val localCount = ping.count // captures `ping.count` (Int) within `open`
                pong =>
                  pong.count  = localCount
              })

              ping.sender.send(box)
            }

          }
      }
    })
  }
}


@RunWith(classOf[JUnit4])
class Spec {

  @Test
  def test(): Unit = {
    // to check result
    val p: Promise[Boolean] = Promise()

    val sys = System()
    val b = sys.actor[Msg](new ActorB(p))
    val a = sys.actor[Msg](new ActorA(b))

    try {
      mkBox[Start] { packed =>
        import packed.access
        val box: packed.box.type = packed.box
        a.send(box)
      }
    } catch {
      case t: Throwable =>
        val res = Await.result(p.future, 2.seconds)
        assert(res)
    }
  }

}
