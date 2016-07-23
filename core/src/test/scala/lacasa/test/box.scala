/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.test.uniqueness

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.{Future, Promise, Await}
import scala.concurrent.duration._

import scala.spores._
import scala.spores.SporeConv._

import lacasa.{System, Box, CanAccess, Actor, ActorRef}
import Box._


class C {
  var f: D = null
  //var count = 0
}

class D {
  var g: C = null
}

sealed abstract class Msg
final case class Start() extends Msg
//final case class Repeat(obj: C) extends Msg

class ActorA(next: ActorRef[C]) extends Actor[Msg] {
  def receive(msg: Box[Msg])(implicit access: CanAccess { type C = msg.C }): Unit = {
    // create box with externally-unique object
    mkBox[C] { packed =>
      implicit val acc = packed.access
      val box: packed.box.type = packed.box

      // initialize object in box
      box.open(spore { obj =>
        val d = new D
        d.g = obj
        obj.f = d
      })

      next.send(box)(spore { () => })
    }
  }
}

class ActorB(p: Promise[Boolean]) extends Actor[C] {
  def receive(msg: Box[C])(implicit access: CanAccess { type C = msg.C }): Unit = {
    msg.open(spore { x =>
      val d = x.f
      // check that `d` refers back to `x`
      p.success(d.g == x)
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
    val b = sys.actor[C](new ActorB(p))
    val a = sys.actor[Msg](new ActorA(b))

    try {
      mkBox[Start] { packed =>
        import packed.access
        val box: packed.box.type = packed.box
        a.send(box)(spore { () => })
      }
    } catch {
      case t: Throwable =>
        val res = Await.result(p.future, 2.seconds)
        assert(res)
    }

  }

}
