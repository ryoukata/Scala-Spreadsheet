// スプレッドシートの定義
import swing._
import event._

/**
* height, widthはセル数を表す
*/
class Spreadsheet(val height: Int, val width: Int) extends ScrollPane {

  val cellModel = new Model(height, width)
  import cellModel._

  // 属性を設定するtableコンポーネント
  val table = new Table(height, width) {
    rowHeight = 25                                      // 表の行の高さ（ポイント指定）
    autoResizeMode = Table.AutoResizeMode.Off           // 表の自動サイズ変更をオフ
    showGrid = true                                     // セルの間にグリッド線を引く
    gridColor = new java.awt.Color(150, 150, 150)       // グリッド線の色をダークグレイに指定

    override def rendererComponent(isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      if(hasFocus) new TextField(userData(row, column))
      else
        new Label(cells(row)(column).toString) {
          xAlignment = Alignment.Right
        }

    def userData(row: Int, column: Int): String = {
      val v = this(row, column)
      if(v == null) "" else v.toString
    }

    reactions += {
      case TableUpdated(table, rows, column) =>
        for (row <- rows)
          cells(row)(column).formula =
            FormulaParsers.parse(userData(row, column))

      case ValueChanged(cell) =>                        // イベントが通知されると、対応するセルにupdateCellメソッドを呼び出し、再描画を要求
        updateCell(cell.row, cell.column)
    }

    for (row <- cells; cell <- row) listenTo(cell)      // すべてのセルにイベントの購読を登録
  }

  // 行番号のヘッダー
  val rowHeader =
    new ListView((0 until height) map (_.toString)) {   // ListViewクラスは０～９９までの数値を表示
      fixedCellWidth = 30                               // セル幅を３０ポイントに固定
      fixedCellHeight = table.rowHeight                 // セルの高さをtableのrowHeightに揃える
    }

  // 以下２つはScrollPaneのフィールド
  viewportView = table                                  // 2つのスクロールバーを指定
  rowHeaderView = rowHeader                             // 左端の行ヘッダーの垂直スクロールバーを指定

}
