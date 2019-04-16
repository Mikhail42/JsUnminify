package org.ionkin

import org.junit.Assert.assertEquals
import org.junit.Test

class JsStringTest {
  @Test
  def jsString() {
    assertEquals(5, new JsString("ss\'s2\'", 2).findRight())
    assertEquals(6, new JsString("ss\"s22\"", 2).findRight())
    assertEquals(6, new JsString("ss\"s22\" 'sdd \"ddfd\" '", 2).findRight())
    assertEquals(20, new JsString("ss\"s22\" 'sdd \"ddfd\" '", 8).findRight())
    assertEquals(18, new JsString("ss\"s22\" 'sdd \"ddfd\" '", 13).findRight())
    assertEquals(20, new JsString("ss\"s22\" \"sdd \'ddfd\' \"", 8).findRight())
    assertEquals(6, new JsString("ss\"s22\" 'sd{(d \"ddfd\" '", 2).findRight())
    assertEquals(22, new JsString("ss\"s22\" 'sd{(d \"ddfd\" '", 8).findRight())
    assertEquals(20, new JsString("ss\"s22\" 'sdd{( \"ddfd\" '", 15).findRight())
    assertEquals(22, new JsString("ss\"s22\" \"sdd{( \'ddfd\' \"", 8).findRight())
  }
}
