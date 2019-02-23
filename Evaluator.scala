// 数式を評価するためのトレイト（Modelクラスに継承される）
trait Evaluator { this: Model =>

  // 評価するメソッド
  def evaluate(e: Formula): Double =
    try {
      e match {
        case Coord(row, column) => cells(row)(column).value

        case Number(v) => v

        case Textual(_) => 0

        case Application(function, arguments) =>
          val argvals = arguments flatMap evalList
          operations(function)(argvals)
      }
    } catch {
      case ex: Exception => Double.NaN
    }

  type Op = List[Double] => Double                              // Op型は演算子型の便利な別名
  val operations = new collection.mutable.HashMap[String, Op]   // 関数名から関数オブジェクトを引き出すマップ

  // 引数に数式をとり、値のリストを返す関数（引数がリストと評価される可能性がある引数を処理）
  private def evalList(e: Formula): List[Double] =
    e match {
      case Range(_, _) => references(e) map (_.value)           // 結果値が範囲が参照するセルの値から構成されるリスト
      case _ => List(evaluate(e))                               // Range以外の引数は、数式の表す1個の値から構成されたリスト
    }

  // 数式が参照するセルを計算する関数
  def references(e: Formula): List[Cell] =
    e match {
      case Coord(row, column) => List(cells(row)(column))

      case Range(Coord(r1, c1), Coord(r2, c2)) =>
        for (row <- (r1 to r2).toList; column <- c1 to c2) yield cells(row)(column)

      case Application(function, arguments) => arguments flatMap references

      case _ => List()
    }

}
