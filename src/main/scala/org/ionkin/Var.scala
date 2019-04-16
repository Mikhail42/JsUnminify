package org.ionkin

object Var {
  val nameLetters: Set[Char] = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ List('$', '_')).toSet
  def findVarPrim(text: String, offset: Int = 0): Option[Int] = {
    val p1 = text.indexOf("var")
    val p2 = text.indexOf("const")
    List(p1, p2).filterNot(_ == -1).sorted.lastOption
  }
  def findMutable(text: String, offset: Int): (Boolean, Int) = text match {
    case x if x.startsWith("var", offset) => (false, offset + 4)
    case x if x.startsWith("const", offset) => (true, offset + 5)
  }
  def findName(text: String, mutOffset: Int): (String, Int) = {
    val lastIndex = text.indexWhere(p => !nameLetters.contains(p), mutOffset)
    (text.substring(mutOffset, lastIndex + 1), lastIndex + 1)
  }
  def findVar(text: String, offset: Int = 0): Option[(Var, Int, Int)] = {
    findVarPrim(text, offset).map(pos => {
      val (mutable, nameOffset) = findMutable(text, offset)

      (???, pos, ???)
    })
  }
}
case class Var(name: String, value: Option[String] = None, mutable: Boolean = true, `type`: String = "any") extends Elem
