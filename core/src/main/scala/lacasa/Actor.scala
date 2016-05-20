package lacasa

import java.util.concurrent.locks.{Lock, ReentrantLock}

import scala.reflect.{ClassTag, classTag}
import scala.concurrent.ExecutionContext
import scala.collection.mutable.Buffer


abstract class Actor[T] {
  self =>

  private val buffer = Buffer.empty[Packed[T]]
  private var idle = true
  private val lock: Lock = new ReentrantLock()

  @volatile
  private[lacasa] var context: ExecutionContext = null

  def receive(msg: Box[T])(implicit cap: CanAccess { type C = msg.C }): Unit

  def self(): ActorRef[T] = {
    new ActorRef(this)
  }

  def exit(): Nothing = {
    throw new NoReturnControl
  }

  // `send` must be strictly internal, since it is passed a `Packed`
  private[lacasa] def send(packed: Packed[T]): Unit = {
    lock.lock()
    try {
      if (idle) {
        idle = false
        val task = new BufferProcessor(packed)
        context.execute(task)
      } else {
        buffer += packed
      }
    } finally {
      lock.unlock()
    }

    // discard stack because of permission transfer
    throw new NoReturnControl
  }

  private class BufferProcessor(packed: Packed[T]) extends Runnable {
    def run(): Unit = {
      val localPacked = packed
      import localPacked.access

      try {
        // process message
        receive(localPacked.box)
      } catch {
        case nrc: NoReturnControl => /* do nothing */
      }

      // check for next message
      lock.lock()
      try {
        if (buffer.isEmpty) {
          idle = true
        } else {
          val task = new BufferProcessor(buffer.remove(0))
          context.execute(task)
        }
      } finally {
        lock.unlock()
      }
    }
  }

}

// Note: class final and constructor private.
final class ActorRef[T] private[lacasa] (instance: Actor[T]) {
  def send(msg: Box[T])(implicit access: CanAccess { type C = msg.C }): Unit =
    // *internally* it is OK to create a `Packed` instance
    instance.send(msg.pack())
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
    new ActorRef(instance)
  }

  def actor[S](creator: => Actor[S]): ActorRef[S] = {
    val instance = creator
    instance.context = context
    new ActorRef(instance)
  }

}
