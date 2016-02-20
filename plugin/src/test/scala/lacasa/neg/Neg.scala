package lacasa.neg

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import lacasa.util._

@RunWith(classOf[JUnit4])
class NegSpec {
  @Test
  def test1() {
    expectError("class") {
      """
        class C {}
        class C {}
      """
    }
  }

  @Test
  def test2() {
    expectError("insecure") {
      """
object C {
  var danger = 5
}

class C {
  val f = 5
  def m(x: Int): Unit = {
    C.danger = 10
  }
}"""
    }
  }

  @Test
  def test3() {
    expectError("class E") {
      """
object C {
  var danger = 5
}

class C {
  val f = 5
  def m(x: Int): Unit = {
    C.danger = 10
  }
}

class E extends D {
  val h = 15
}

class D extends C {
  val g = 10
}"""
    }
  }

}
