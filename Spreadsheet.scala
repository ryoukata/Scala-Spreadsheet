// スプレッドシートの定義
import swing._

/**
* height, widthはセル数を表す
*/
class Spreadsheet(val height: Int, val width: Int) extends ScrollPane {

  // 属性を設定するtableコンポーネント
  val table = new Table(height, width) {
    rowHeight = 25                                      // 表の行の高さ（ポイント指定）
    autoResizeMode = Table.AutoResizeMode.Off           // 表の自動サイズ変更をオフ
    showGrid = true                                     // セルの間にグリッド線を引く
    gridColor = new java.awt.Color(150, 150, 150)       // グリッド線の色をダークグレイに指定
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
