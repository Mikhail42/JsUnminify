package org.ionkin

import org.junit.Assert.assertEquals
import org.junit.Test

class ContainerTest {

  @Test
  def brace() {
    assertEquals("check right pos {", 3 + "{ var a = 5 ".length, Brace("sd { var a = 5 }", 3).rightPos)
    assertEquals(" var a = 5 ", Brace("sd { var a = 5 }", 3).inner)
    assertEquals("check right pos (", 3 + "( var a = 5 ".length, RoundBracket("sd ( var a = 5 )", 3).rightPos)
    assertEquals(" var a = 5 ", RoundBracket("sd ( var a = 5 )", 3).inner)
    assertEquals(" var a = 5, b = {c: 5} ", RoundBracket("sd ( var a = 5, b = {c: 5} )", 3).inner)

    assertEquals(" var a = 5, b = (true || false) && 's})ds' && \"s})\"",
      RoundBracket("sd ( var a = 5, b = (true || false) && 's})ds' && \"s})\")dsd",  3).inner)
    assertEquals(" var a = 5, b = (true || false) && 's}}ds' && \"s})\"",
      Brace("sd { var a = 5, b = (true || false) && 's}}ds' && \"s})\"})dsd", 3).inner)
  }
}
