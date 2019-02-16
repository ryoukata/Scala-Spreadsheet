// スプレッドシートの数式を表す型を定義するクラス
trait Formula

// A3などのセル座標
case class Coord(row: Int, column: Int) extends Formula {
  override def toString = ('A' + column).toChar.toString + row
}

// A3:B17などのセル範囲
case class Range(c1: Coord, c2: Coord) extends Formula {
  override def toString = c1.toString + ":" + c2.toString
}

// 浮動小数点の数字
case class Number(value: Double) extends Formula {
  override def toString = value.toString
}

// 賛成数、Totalなどのテキストによるラベル
case class Textual(value: String) extends Formula {
  override def toString = value
}

// 関数の適用
case class Application(function: String, arguments: List[Formula]) extends Formula {
  override def toString = function + arguments.mkString("(", ", ", ")")
}

object Empty extends Textual("")
