// 入力した文字列からFourmulaの木構造を構築するクラス
import scala.util.parsing.combinator._

object FormulaParsers extends RegexParsers {

  def ident: Parser[String] = """[a-zA-Z_]\w*""".r        // 識別子のための補助パーサー
  def decimal: Parser[String] = """-?\d+(\.\d*)?""".r     // 10進数のための補助パーサー

  // セル座標を認識する（英字と数字に分割し、列と行を表す添え字に変換）
  def cell: Parser[Coord] =
    """[A-Za-z]\d+""".r ^^ {s =>
      val column = s.charAt(0).toUpper - 'A'
      val row = s.substring(1).toInt
      Coord(row, column)
    }

  // セルの範囲を認識する
  def range: Parser[Range] =
    cell~":"~cell ^^ {
      case c1~":"~c2 => Range(c1, c2)
    }

  // 10進数を認識し、読みだした値をDoubleに変換してNumberクラスのインスタンスでラップする
  def number: Parser[Number] =
    decimal ^^ (d => Number(d.toDouble))

  // 関数適用を認識する（識別子の後ろにカッコで囲われた引数式のリストが続く形式にしている）
  def application: Parser[Application] =
    ident~"("~repsep(expr, ",")~")" ^^ {
      case f~"("~ps~")" => Application(f, ps)
    }

  // =の後ろに続くトップレベル数式、関数の引数も数式として認識する（セル、セル範囲、数値、関数適用のいずれかとして定義される）
  def expr: Parser[Formula] =
    range | cell | number | application

  // 先頭が等号になっていない任意の入力文字列を認識する（先頭が＝の文字列は数式とみなす）
  def textual: Parser[Textual] =
    """[^=].*""".r ^^ Textual

  // セルに対するすべての有効な入力を認識（number, textual,先頭が符号の数式のどれか）
  def formula: Parser[Formula] =
    number | textual | "="~>expr

  // 入力文字列をFormulaの木構造に変換
  def parse(input: String): Formula =
    parseAll(formula, input) match {
      case Success(e, _) => e
      case f: NoSuccess => Textual("[" + f.msg + "]")
    }

}
