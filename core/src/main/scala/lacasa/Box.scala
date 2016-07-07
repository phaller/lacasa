package lacasa

import scala.reflect.{ClassTag, classTag}

import scala.spores._

import scala.util.control.ControlThrowable


private class NoReturnControl extends ControlThrowable {
  // do not fill in stack trace for efficiency
  final override def fillInStackTrace(): Throwable = this
}

/*sealed*/ class CanAccess {
  type C
}

object Box {
  /*private*/ def make[T](init: T): Box[T] = {
    def internal[S]: Box[T] = {
      new Box[T](init) {
        type C = S
      }
    }
    internal[Int]
  }

  def mkBox[T: ClassTag](spore: Packed[T] => Unit): Unit = {
    val cl = classTag[T].runtimeClass
    val instance: T = cl.newInstance().asInstanceOf[T]
    val theBox = new Box[T](instance)
    val packed = theBox.pack()
    spore(packed)
    throw new NoReturnControl
  }
}

sealed trait OnlyNothing[T]

object OnlyNothing {
  implicit val onlyNothing: OnlyNothing[Nothing] = new OnlyNothing[Nothing] {}
  implicit val intOnlyNothing: OnlyNothing[Int] = new OnlyNothing[Int] {}
  implicit def actorRefOnlyNothing[T]: OnlyNothing[ActorRef[T]] = new OnlyNothing[ActorRef[T]] {}
  implicit def tuple2OnlyNothing[T, S](implicit one: OnlyNothing[T], two: OnlyNothing[S]): OnlyNothing[(T, S)] = new OnlyNothing[(T, S)] {}
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

  def open(fun: Spore[T, Unit])(implicit access: CanAccess { type C = self.C }, noCapture: OnlyNothing[fun.Captured]): Box[T] = {
    fun(instance)
    self
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
      fun(new Packed[S] {
        val box: Box[S] = Box.make[S](null.asInstanceOf[S])
        val access = new CanAccess { type C = box.C }
      })
    } else {
      // we can do this inside the `lacasa` package :-)
      implicit val localAcc = new CanAccess { type C = prev.C }
      implicit def fakeOnlyNothing[T] = new OnlyNothing[T] {}
      prev.open(spore {
        val localFun = fun
        (prevValue: S) =>
          val b = Box.make[S](prevValue)
          val packed = b.pack()
          localFun(packed)
      })
    }
    throw new NoReturnControl
  }
}

sealed trait Packed[+T] {
  val box: Box[T]
  implicit val access: CanAccess { type C = box.C }
}
