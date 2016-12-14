/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa

import scala.reflect.{ClassTag, classTag}

import scala.spores._

import scala.util.control.ControlThrowable


private class NoReturnControl extends ControlThrowable {
  // do not fill in stack trace for efficiency
  final override def fillInStackTrace(): Throwable = this
}

/* Class *must* be sealed, so that no class outside this
 * source file can mix in CanAccess.
 */
sealed class CanAccess {
  type C
}

object Box {

  def mkBox[T: ClassTag](spore: Packed[T] => Unit): Nothing = {
    val cl = classTag[T].runtimeClass
    val instance: T = cl.newInstance().asInstanceOf[T]
    val theBox = new Box[T](instance)
    val packed = theBox.pack()
    spore(packed)
    throw new NoReturnControl
  }

  // marker method as escape hatch for ControlThrowable checker
  def uncheckedCatchControl: Unit = {}

  /* for internal use only! */
  private[lacasa] def make[T](init: T): Box[T] = {
    def internal[S]: Box[T] = {
      new Box[T](init) {
        type C = S
      }
    }
    internal[Int]
  }

  private[lacasa] def packedNull[S] = new Packed[S] {
    val box: Box[S] = Box.make[S](null.asInstanceOf[S])
    val access = new CanAccess { type C = box.C }
  }

}

sealed trait Safe[T]

object Safe {
  implicit val nothingIsSafe: Safe[Nothing] = new Safe[Nothing] {}
  implicit val intIsSafe: Safe[Int] = new Safe[Int] {}
  implicit val stringIsSafe: Safe[String] = new Safe[String] {}
  implicit def actorRefIsSafe[T]: Safe[ActorRef[T]] = new Safe[ActorRef[T]] {}
  implicit def tuple2IsSafe[T, S](implicit one: Safe[T], two: Safe[S]): Safe[(T, S)] = new Safe[(T, S)] {}
}

sealed class Box[+T] private (private val instance: T) {
  self =>

  type C

  // trusted operation
  private[lacasa] def pack(): Packed[T] = {
    new Packed[T] {
      val box: Box[T] = self
      implicit val access: CanAccess { type C = box.C } =
        new CanAccess { type C = box.C }
    }
  }

  def open(fun: Spore[T, Unit])(implicit access: CanAccess { type C = self.C }, noCapture: Safe[fun.Captured]): Box[T] = {
    fun(instance)
    self
  }

  def extract[S: Safe](fun: Spore[T, S])(implicit access: CanAccess { type C = self.C }, noCapture: Safe[fun.Captured]): S = {
    fun(instance)
  }

  // swap field
  // `select` must have form `_.f` (LaCasa plugin checks)
  // for now: `assign` must have form `(x, y) => x.f = y`
  def swap[S](select: T => Box[S])(assign: (T, Box[S]) => Unit, newBox: Box[S])(
    fun: Spore[Packed[S], Unit] { type Excluded = newBox.C })(
    implicit access: CanAccess { type C = newBox.C }): Unit = {
    val prev = select(instance)
    // do the assignment
    assign(instance, newBox)
    // pass `prev` using fresh permission to continuation
    if (prev == null) {
      fun(Box.packedNull[S])
    } else {
      // we can do this inside the `lacasa` package :-)
      implicit val localAcc = new CanAccess { type C = prev.C }
      implicit def fakeSafe[T] = new Safe[T] {}
      prev.open(spore { // can simplify this: have access to prev.instance!
        val localFun = fun
        (prevValue: S) =>
          val b = Box.make[S](prevValue)
          val packed = b.pack()
          localFun(packed)
      })
    }
    throw new NoReturnControl
  }

  /* Captures the `consumed` box, and merges it into `self`.
   * In the continuation `fun`, box `self` is open.
   *
   * The argument `assign` must have the form `(x, y) => x.f = y`.
   */
  def capture[S](consumed: Box[S])(assign: (T, S) => Unit)(
    fun: Spore[Packed[T], Unit] /*{ type Excluded = consumed.C }*/)( // TODO: fix spores
    implicit access: CanAccess { type C = self.C },
      accessConsumed: CanAccess { type C = consumed.C }): Nothing = {

    // do the assignment
    assign(instance, consumed.instance)

    // invoke continuation
    fun(pack())

    throw new NoReturnControl
  }

}

sealed trait Packed[+T] {
  val box: Box[T]
  implicit val access: CanAccess { type C = box.C }
}
