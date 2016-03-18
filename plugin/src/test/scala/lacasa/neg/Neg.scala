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

  @Test
  def test4() {
    expectError("insecure") {
      """
object C {
  val nodanger = 10
  var danger = 5
}

class C {
  val f = 5
  def m(x: Int): Unit = {
    val v1 = C.nodanger
    val v2 = C.danger
  }
}"""
    }
  }

  @Test
  def test5() {
    expectError("insecure") {
      """
class C {
  val f = 5
  def m(x: Int): Unit = {
    val v1 = C.someMethod(5)
  }
}
object C {
  var danger = 5
  def someMethod(x: Int) = x + danger
}"""
    }
  }

  @Test
  def test6() {
    expectError("insecure") {
      """
object C {
  var danger = 5
  def someMethod(x: Int) = x + danger
}
class C {
  val f = 5
  def m(x: Int): Unit = {
    val v1 = C.someMethod(5)
  }
}"""
    }
  }

  @Test
  def test7() {
    expectError("class D") {
      """
object C {
  var danger = 5
}
class C(x: Int) {
  def m(y: Int): Unit = {
    val v1 = C.danger + x + y
  }
}
class D {
  def m2(y: Int): Unit = {
    val c = new C(y)
  }
}"""
    }
  }

  @Test
  def test8() {
    expectError("class D") {
      """
class D {
  def m2(y: Int): Unit = {
    val c = new C(y)
  }
}
object C {
  var danger = 5
}
class C(x: Int) {
  def m(y: Int): Unit = {
    val v1 = C.danger + x + y
  }
}"""
    }
  }

  @Test
  def test9() {
    expectError("class D") {
      """
object C {
  var danger = 5
}
class C(x: Int) {
  def m(y: Int): Unit = {
    val v1 = C.danger + x + y
  }
}
class E[T] { var x: T = _ }
class D {
  def m2(y: Int): Unit = {
    val inst = new E[C]
  }
}"""
    }
  }

  @Test
  def test10() {
    expectError("class D") {
      """
class E[T] { var x: T = _ }
class D {
  def m2(y: Int): Unit = {
    val inst = new E[C]
  }
}
object C {
  var danger = 5
}
class C(x: Int) {
  def m(y: Int): Unit = {
    val v1 = C.danger + x + y
  }
}"""
    }
  }

}
