// セルの座標位置を保持するモデルクラス
class Model(val height: Int, val width: Int) {
  case class Cell(row: Int, column: Int)
  val cells = Array.ofDim[Cell](height, width)
  val formula: Formula = Empty                      // スプレッドシートにパーサを統合

  override def toString = formula.toString
  for (i <- 0 until height; j <- 0 until width)
    cells(i)(j) = new Cell(i, j)

}
