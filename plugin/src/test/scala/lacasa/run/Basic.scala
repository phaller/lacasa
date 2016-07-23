/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa.run

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class BasicSpec {

  @Test
  def test1(): Unit = {
    assert(true, "this should not fail!")
  }

}
