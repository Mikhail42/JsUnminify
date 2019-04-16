package org.ionkin

object JsString {
  val chars = List('\'', '\"', '`')
}
class JsString(text: String, offset: Int = 0) {
  val c = text.charAt(offset)
  if (!JsString.chars.contains(c)) throw new Exception(s"Illegal JsString: text=${text}, offset=${offset}")

  def findRight(): Int = {
    var pos = offset + 1
    while (text.charAt(pos) != c) {
      pos += 1
    }
    pos
  }
}
