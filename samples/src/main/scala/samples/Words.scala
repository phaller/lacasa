/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.samples.words

import lacasa.{Box, CanAccess, Actor, ActorRef, LaCasaApp, LaCasa, doNothing}
import Box._  // import `mkBox`


class Words {
  var list: List[String] = List()
}

class ActorA extends Actor[Words] {
  override def receive(box: Box[Words])
                 (implicit acc: CanAccess { type C = box.C }) {
    box.open({ words =>
      // remove first two words from list
      val removedWords = words.list.take(2)
      words.list = words.list.drop(2)
      println("Number of words left: " + words.list.size)
    })
  }
}

class Start {
  var next: ActorRef[Words] = _
}

class ActorB extends Actor[Start] {
  override def receive(box: Box[Start])
                 (implicit acc: CanAccess { type C = box.C }) {
    box open { start =>
      // LaCasa checks that `Words` is an ocap class
      mkBox[Words] { packed =>
        implicit val acc = packed.access  // make permission available implicitly

        // open box to initialize words
        packed.box.open({ words =>
          words.list = List("box", "with", "sequence", "of", "words")
        })

        start.next.send(packed.box)({ /* do nothing */ doNothing.consume(packed.box) })
      }
    }
  }
}

object Example extends LaCasaApp {
  // terminate main thread after 1000 millis
  override val terminateAfter: Long = 1000

  def lcMain(args: Array[String]): Unit = {
    val first = LaCasa.system.actor[ActorB, Start]

    // LaCasa checks that `Start` is an ocap class
    mkBox[Start] { packed =>
      implicit val acc = packed.access

      // initialize object in box
      packed.box.open({ obj =>
        obj.next = LaCasa.system.actor[ActorA, Words]
      })

      first.send(packed.box)({ /* do nothing */ doNothing.consume(packed.box) })
    }
  }

}
