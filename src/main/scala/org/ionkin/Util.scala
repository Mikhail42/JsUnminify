package org.ionkin

object Util {

  def spaceCountBefore(text: String, offset: Int): Int = {
    var r = 0
    while (offset - r > 0 && text.charAt(offset - r - 1) == ' ') {
      r += 1
    }
    r
  }

  def spaceBefore(text: String, offset: Int): String = new String(Array.fill[Char](spaceCountBefore(text, offset))(' '))

  def replace(text: String, from: Int, until: Int, newStr: String): String =
    text.substring(0, from) + newStr + text.substring(until)
}
