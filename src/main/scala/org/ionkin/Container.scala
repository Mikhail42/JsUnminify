package org.ionkin

trait Container {
  def inner: String
  /*def globalVars: List[Var] = List()
  def localVars: List[Var] = List()
  def vars: List[Var] = globalVars ++ localVars
  def funs: List[Function] = List()
  def elems: List[Elem] = vars ++ funs*/
}


class Pair(text: String, offset: Int, left: Char, right: Char) extends Container {
  if (!text.startsWith(left.toString, offset)) throw new Exception(s"Illegal pair: text=${text}, offset=${offset}")

  val rightPos: Int = {
    var (leftCount, rightCount, pos) = (1, 0, offset + 1)
    while (leftCount > rightCount) {
      text.charAt(pos) match {
        case x if x == left => leftCount += 1
        case x if x == right => rightCount += 1
        case x if JsString.chars.contains(x) => pos = new JsString(text, pos).findRight()
        case x =>
      }
      pos += 1
    }
    pos - 1
  }
  val rightPosInInner: Int = rightPos - offset - 1
  val inner: String = text.substring(offset + 1, rightPos)
}

case class Brace(text: String, offset: Int = 0) extends Pair(text, offset, '{', '}')
case class RoundBracket(text: String, offset: Int = 0) extends Pair(text, offset,'(', ')')
