/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa


object Utils {

  def loop[T](iter: Iterator[T])(fun: T => Nothing): Nothing = if (iter.hasNext) {
    val elem = iter.next()
    try {
      fun(elem)
    } catch {
      case _: NoReturnControl =>
        Box.uncheckedCatchControl
        loop(iter)(fun)
    }
  } else {
    throw new NoReturnControl
  }

  def loopAndThen[T, S](iter: Iterator[T])(fun: T => Nothing)(cont: () => S): Nothing = if (iter.hasNext) {
    val elem = iter.next()
    try {
      fun(elem)
    } catch {
      case _: NoReturnControl =>
        Box.uncheckedCatchControl
        loopAndThen(iter)(fun)(cont)
    }
  } else {
    cont()
    throw new NoReturnControl
  }

}
