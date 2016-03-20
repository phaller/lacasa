package lacasa

import scala.reflect.{ClassTag, classTag}


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

  def box[T: ClassTag](spore: Packed[T] => Unit): Unit = {
    val cl = classTag[T].runtimeClass
    val instance: T = cl.newInstance().asInstanceOf[T]
    val theBox = new Box[T](instance)
    val packed = theBox.pack()
    spore(packed)
  }
}

sealed class Box[T] private (private val instance: T) {
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

  def open(fun: T => Unit)(implicit access: CanAccess { type C = self.C }): Box[T] = {
    fun(instance)
    self
  }
}

sealed trait Packed[T] {
  val box: Box[T]
  implicit val access: CanAccess { type C = box.C }
}
