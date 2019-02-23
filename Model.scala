// セルの座標位置を保持するモデルクラス
import swing._                                        // イベントサービスに直接アクセスするためインポート

class Model(val height: Int, val width: Int) extends Evaluator with Arithmetic {
  case class Cell(row: Int, column: Int) extends Publisher {        // イベントを発行するためにPublisherクラスを継承
    override def toString = formula match {
      case Textual(s) => s
      case _  => value.toString
    }

    private var v: Double = 0
    def value: Double = v
    def value_=(w: Double) = {                      // value_=セッターはパブリッシュ/サブスクライブフレームワークの発行者の役割を果たす
      if (!(v == w || v.isNaN && w.isNaN)) {          // 新しい値が元の値と異なる場合は、セル自身を引数としてValueChangedイベントを発行
        v = w
        publish(ValueChanged(this))
      }
    }

    private var f: Formula = Empty
    def formula: Formula = f
    def formula_=(f: Formula) = {                   // formula_=セッターは購読者の役割をはたす
      for (c <- references(formula)) deafTo(c)        // 従来の数式が参照していたすべてのセルに対し、購読を解除
      this.f = f                                      // 非公開変数fに新しい数式を代入
      for (c <- references(formula)) listenTo(c)      // 新しい数式が参照するすべてのセルに対し、イベント通知の購読を登録
      value = evaluate(f)                             // 新しい数式で再計算
    }

    reactions += {
      case ValueChanged(_) => value = evaluate(formula)
    }
  }

  case class ValueChanged(cell: Cell) extends event.Event

  val cells = Array.ofDim[Cell](height, width)
  for (i <- 0 until height; j <- 0 until width)
    cells(i)(j) = new Cell(i, j)

}
