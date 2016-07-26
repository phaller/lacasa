/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.run

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ControlSpec {
  import scala.util.control.ControlThrowable
  import lacasa.Box._

  @Test
  def test1(): Unit = {
    println("run.ControlSpec.test1")
    val res = try { 5 } catch {
      case c: ControlThrowable =>
        throw c
      case t: Throwable =>
        println("hello")
    }
    assert(res == 5, "this should not fail")
  }

}
