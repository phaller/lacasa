/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa


/**
  * Application trait for LaCasa applications. This trait should
  * be used instead of Scala's standard `App` trait.
  *
  * Extending this trait, and implementing its `lcMain` method,
  * provides an application with a `main` method suitable for
  * LaCasa's continuation terms (`mkBox`, `capture`, `swap`, etc.).
  */
trait LaCasaApp {

  val terminateAfter: Long = 0L

  def lcMain(args: Array[String]): Unit

  def main(args: Array[String]): Unit = try {
    lcMain(args)
  } catch {
    case _: NoReturnControl =>
      Box.uncheckedCatchControl
      Thread.sleep(terminateAfter)
  }

}
