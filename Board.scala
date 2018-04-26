import scala.util.Random
import scala.sys.process._

object Board {
  private var boardMatrix: IndexedSeq[Cell] = _
  var width: Int = 20
  var height: Int = 20
  var fps = 20;
  def init: Unit = {
    boardMatrix = for (x <- -width / 2 to width / 2; y <- -height / 2 to height / 2 if Random.nextBoolean()) yield Cell(y, x)
  }

  def isAlive(cell: Cell): Boolean = boardMatrix exists (c =>  c == cell)

  def isAlive(x: Int, y: Int): Boolean = boardMatrix exists  (cell =>  cell.x == x && cell.y == y)

  def neighbours(cell: Cell, keepCore: Boolean = true): IndexedSeq[Cell] = for (x <- cell.x - 1 to cell.x + 1; y <- cell.y - 1 to cell.y + 1 if keepCore || !(cell.x == x && cell.y == y)) yield Cell(x, y)

  def draw = {
    print("\033[H\033[2J")
    for (y <- -width to width ; x <- -height to height) {
      var cell = if (isAlive(x, y)) s"o " else s"  "

      if (x == height)
        cell = s"$cell \n"

      print(cell)
    }
  }

  def survives(cell: Cell): Boolean = {
    val n = neighbours(cell, false) count isAlive

    if (!isAlive(cell))
      n == 3
    else
      n == 2 || n == 3
  }

  def next(board: IndexedSeq[Cell]): IndexedSeq[Cell] = { for (cell <- board; n: Cell <- neighbours(cell)) yield n }.distinct filter survives

  def main(args: Array[String]): Unit = {
    init

    while(true) {
      boardMatrix = next(boardMatrix)
      draw
      Thread.sleep(1000 / fps)
    }
  }
}

case class Cell(x: Int, y: Int) {
  override def toString: String = s"$x:$y"
}