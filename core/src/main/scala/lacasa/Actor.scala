/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa

import java.util.concurrent.locks.{Lock, ReentrantLock}

import scala.reflect.{ClassTag, classTag}
import scala.concurrent.ExecutionContext
import scala.collection.mutable.Buffer

import scala.spores.{NullarySpore, Spore}


object doNothing {
  def consume[T](box: Box[T]): NullarySpore[Unit] { type Excluded = box.C } =
    new NullarySpore[Unit] {
      type Captured = Nothing
      type Excluded = box.C
      def apply(): Unit = {}
    }
}

object sleep {
  def consume[T](box: Box[T])(millis: Long): NullarySpore[Unit] { type Excluded = box.C } =
    new NullarySpore[Unit] {
      type Captured = Nothing
      type Excluded = box.C
      def apply(): Unit = {
        Thread.sleep(millis)
      }
    }
}

/**
  * LaCasa guarantees isolation of actors even in the presence of a
  * shared heap and mutable objects sent as messages. However, this
  * requires an integration of LaCasa's abstractions, boxes and
  * permissions, with actors.
  *
  * LaCasa provides prototype `Actor` and `ActorRef` library classes,
  * but the intended usage consists of a bridge between LaCasa and
  * Scala's standard actor library, Akka.
  *
  * LaCasa introduces two main changes to the `Actor` and `ActorRef`
  * library classes: first, actors send and receive boxes of type
  * `Box[T]`, rather than direct object references. (LaCasa's type
  * system enforces strong encapsulation properties for boxes.)
  * Second, the type of the `receive` method is changed to
  * additionally include an implicit permission parameter.
  */
abstract class Actor[T] {

  private val buffer = Buffer.empty[Packed[T]]
  private var idle = true
  private val lock: Lock = new ReentrantLock()

  @volatile
  private[lacasa] var context: ExecutionContext = null

  def receive(msg: Box[T])(implicit acc: CanAccess { type C = msg.C }): Unit

  final val self: ActorRef[T] = new InternalActorRef(this)

  def exit(): Nothing = {
    throw new NoReturnControl
  }

  // swap for accessing fields of type Box[S]
  def swap[S](select: => Box[S])(assign: Box[S] => Unit, newBox: Box[S])(
    fun: Spore[Packed[S], Unit] { type Excluded = newBox.C })(
    implicit access: CanAccess { type C = newBox.C }): Unit = {

    val prev = select

    // do the assignment
    assign(newBox)

    // invoke continuation
    fun(
      if (prev == null) Box.packedNull[S] else prev.pack()
    )

    throw new NoReturnControl
  }

  // `send` must be strictly internal, since it is passed a `Packed`
  private[lacasa] def send(packed: Packed[T])(cont: () => Unit): Nothing = {
    lock.lock()
    try {
      if (idle) {
        idle = false
        val task = new MessageHandlerTask(this, packed)
        context.execute(task)
      } else {
        buffer += packed
      }
    } finally {
      lock.unlock()
    }

    // invoke continuation
    cont()

    // discard stack because of permission transfer
    throw new NoReturnControl
  }

  private[lacasa] def checkForNextMessage(): Unit = {
    lock.lock()
    try {
      if (buffer.isEmpty) {
        idle = true
      } else {
        val task = new MessageHandlerTask(this, buffer.remove(0))
        context.execute(task)
      }
    } finally {
      lock.unlock()
    }
  }
}

private[lacasa] class MessageHandlerTask[T](
                        receiver: Actor[T],
                        packed: Packed[T]) extends Runnable {
  def run(): Unit = {
    // process message in 'packed' object
    try {
      // process message
      receiver.receive(packed.box)(packed.access)
    } catch {
      case nrc: NoReturnControl => /* do nothing */
    }
    // check for next message
    receiver.checkForNextMessage()
  }
}

/**
  * LaCasa guarantees isolation of actors even in the presence of a
  * shared heap and mutable objects sent as messages. However, this
  * requires an integration of LaCasa's abstractions, boxes and
  * permissions, with actors.
  *
  * LaCasa provides prototype `Actor` and `ActorRef` library classes,
  * but the intended usage consists of a bridge between LaCasa and
  * Scala's standard actor library, Akka.
  *
  * LaCasa introduces two main changes to the `Actor` and `ActorRef`
  * library classes: first, actors send and receive boxes of type
  * `Box[T]`, rather than direct object references. (LaCasa's type
  * system enforces strong encapsulation properties for boxes.)
  * Second, the type of the `receive` method is changed to
  * additionally include an implicit permission parameter.
  */
abstract class ActorRef[T] {
  def send(msg: Box[T])(cont: NullarySpore[Unit] { type Excluded = msg.C })
          (implicit acc: CanAccess { type C = msg.C }): Nothing
}

// Note: class final and constructor private.
final class InternalActorRef[T] private[lacasa] (instance: Actor[T]) extends ActorRef[T] {
  override def send(msg: Box[T])(cont: NullarySpore[Unit] { type Excluded = msg.C })
          (implicit acc: CanAccess { type C = msg.C }): Nothing =
    // *internally* it is OK to create a `Packed` instance
    instance.send(msg.pack())(cont)
}


final class Agent[T] private[lacasa] (private[lacasa] var instance: T)(implicit context: ExecutionContext) {
  self =>

  private val buffer = Buffer.empty[Action[_, T]]
  private var idle = true
  private val lock: Lock = new ReentrantLock() // guards `instance`, `buffer`, and `idle`

  def send[S](action: Action[S, T]): Unit = {
    lock.lock()
    try {
      if (idle) {
        idle = false
        val task = new BufferProcessor(action, instance)
        context.execute(task)
      } else {
        buffer += action
      }
    } finally {
      lock.unlock()
    }
  }

  private class BufferProcessor(action: Action[_, T], current: T) extends Runnable {
    def run(): Unit = {
      val newInstance = action.apply(current)
      lock.lock()
      try {
        instance = newInstance
        if (buffer.isEmpty) {
          idle = true
        } else {
          val task = new BufferProcessor(buffer.remove(0), instance)
          context.execute(task)
        }
      } finally {
        lock.unlock()
      }
    }
  }

}

class Action[S, T] private[lacasa] (fun: (Packed[S], T) => T, packed: Packed[S]) {
  private[lacasa] def apply(arg: T): T =
    fun(packed, arg)
}

object Action {
  def make[S, T](fun: (Packed[S], T) => T, box: Box[S])(implicit access: CanAccess { type C = box.C }): Action[S, T] = {
    new Action(fun, box.pack())
  }
}


object System {
  /** Creates a runtime system for actors and agents.
   */
  def apply()(implicit context: ExecutionContext): System = {
    new System
  }
}

class System private[lacasa] (implicit context: ExecutionContext) {

  def agent[T](init: T): Agent[T] = {
    new Agent(init)
  }

  def actor[T <: Actor[S] : ClassTag, S]: ActorRef[S] = {
    val cl = classTag[T].runtimeClass
    val instance = cl.newInstance().asInstanceOf[Actor[S]]
    instance.context = context
    new InternalActorRef(instance)
  }

  def actor[S](creator: => Actor[S]): ActorRef[S] = {
    val instance = creator
    instance.context = context
    new InternalActorRef(instance)
  }

}

object LaCasa {
  val system = System()(ExecutionContext.Implicits.global)
}
