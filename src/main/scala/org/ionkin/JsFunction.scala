package org.ionkin

import scala.collection.mutable

case class Return(superBody: Brace) {
  val innerOfBody: String = superBody.inner
  val offsetOfReturnInInnerOfBodyOpt: Option[Int] = JsFunction(superBody, None).offsetOfReturnInInner()

  def hasReturn: Boolean = offsetOfReturnInInnerOfBodyOpt.isDefined

  lazy val beforeReturn: String = innerOfBody.substring(0, offsetOfReturnInInnerOfBodyOpt.get)
  lazy val space = Util.spaceBefore(innerOfBody, offsetOfReturnInInnerOfBodyOpt.get)

  lazy val unminifiedReturn: String = {
    val innerOfReturn: String = innerOfBody.substring(offsetOfReturnInInnerOfBodyOpt.get + "return ".length, superBody.rightPosInInner)
    val linesBuf: mutable.Buffer[String] = mutable.Buffer()
    var pos = 0
    var startPos = 0
    while (pos < innerOfReturn.length) {
      innerOfReturn.charAt(pos) match {
        case '(' => pos = RoundBracket(innerOfReturn, pos).rightPos
        case '{' => pos = Brace(innerOfReturn, pos).rightPos
        case ',' => {
          linesBuf += innerOfReturn.substring(startPos, pos).trim
          startPos = pos + 1
        }
        case x if JsString.chars.contains(x) => pos = new JsString(innerOfReturn, pos).findRight()
        case _ =>
      }
      pos += 1
    }
    linesBuf += innerOfReturn.substring(startPos, pos).trim
    val init = linesBuf.init.mkString(";\n" + space)
    init + (if (linesBuf.size > 1) ";\n" + space else "") + "return " + linesBuf.last + ";\n" + space.substring(0, space.length - 2)
  }

  def unminifiedBodyWithReturn(): String = beforeReturn + unminifiedReturn
}

case class JsFunction(var body: Brace, name: Option[String] = None) extends Elem {
  def inner(): String = body.inner

  def offsetOfReturnInInner(): Option[Int] = {
    var pos = 0
    while (pos < this.inner.length) {
      inner.charAt(pos) match {
        case '(' => pos = RoundBracket(inner, pos).rightPos
        case '{' => pos = Brace(inner, pos).rightPos
        case x if JsString.chars.contains(x) => pos = new JsString(inner, pos).findRight()
        case x if inner.startsWith("return", pos) => return Some(pos)
        case _ =>
      }
      pos += 1
    }
    None
  }

  /*def findInnerBodies(): List[Brace] = {
    val bodiesBuf = mutable.Buffer[Brace]()
    var pos = 0
    while (pos < inner().length) {
      inner().charAt(pos) match {
        case '(' => pos = RoundBracket(inner(), pos).rightPos
        case '{' => {
          val b = Brace(inner(), pos)
          bodiesBuf += b
          pos = b.rightPos
        }
        case x if JsString.chars.contains(x) => pos = new JsString(inner(), pos).findRight()
        case _ =>
      }
      pos += 1
    }
    bodiesBuf.toList
  }
*/
  def findNextInnerBody(offset: Int): Option[Brace] = {
    var pos = 0
    while (pos < inner().length) {
      inner().charAt(pos) match {
        case '(' => pos = RoundBracket(inner(), pos).rightPos
        case '{' => return Some(Brace(inner(), pos))
        case x if JsString.chars.contains(x) => pos = new JsString(inner(), pos).findRight()
        case _ =>
      }
      pos += 1
    }
    None
  }

  def findNextBodyWithReturn(offset: Int): Option[Return] =
    findNextInnerBody(offset) match {
      case Some(b) => {
        val r = Return(b)
        if (r.hasReturn) Some(r)
        else findNextBodyWithReturn(r.superBody.rightPos)
      }
      case None => None
    }

  def findBodyAndReplace(offset: Int) {
    val fOpt = findNextBodyWithReturn(offset)
    fOpt.map(x => JsFunction(x.superBody)).foreach(f => {
      val newF = f.unminifyBody()
      this.body = Brace(Util.replace(body.text, f.body.offset + 1, f.body.rightPos, newF), body.offset)
      findBodyAndReplace(f.body.rightPos)
    })
  }

  def unminifyBody(): String = {
    val ret = Return(body)
    val unBody = ret.unminifiedBodyWithReturn()
    this.body = Brace(Util.replace(body.text, body.offset + 1, body.rightPos, unBody), body.offset)
    findBodyAndReplace(this.body.offset + 1)
    inner()
  }
}
