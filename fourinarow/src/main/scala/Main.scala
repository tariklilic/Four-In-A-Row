import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object Main {
  def main(args: Array[String]): Unit = {
    var player: Int = 1
    var j: Int = 1

    print("Enter number of rows: ")
    val rows: Int = readInt
    print("Enter number of columns: ")
    val cols: Int = readInt

    val gameBoard = Array.ofDim[String](rows, cols)

    for (i <- gameBoard.indices) {
      for (j <- gameBoard(i).indices) {
        if (gameBoard(i)(j) == null) {
          gameBoard(i)(j) = "○"
        }
      }
    }

    while(true) {
      print("Now Playing: " + player + " Enter column: ")
      var selectedCol: Int = readInt

      if (player == 1) {
        player = 2
      } else {
        player = 1
      }

      //FOR LOOP BY SHAZ
      for (i <- rows to 1 by -1) {
        if (gameBoard(rows - i)(selectedCol - 1) == "○") {
          j = i
        }
      }

      drop(player, selectedCol, rows - j, cols, gameBoard)
      println(check(gameBoard))

      val gameBoardtoString = gameBoard.map(_.mkString(" ")).mkString("\n")
      println(gameBoardtoString)
    }
  }

  def drop(player: Int, selectedCol: Int, row: Int, col: Int, gameBoard: Array[Array[String]]): Array[Array[String]] = {
    var res: String = " "

    if (player == 1) {
      res = "⬤"
    } else {
      res = "◍"
    }

    gameBoard(row)(selectedCol - 1) = res

    gameBoard
  }

  def check(board: Array[Array[String]]): Option[String] = {
    // Check for horizontal wins
    for (row <- board) {
      for (i <- 0 to 3) {
        if (row(i) == row(i + 1) && row(i + 1) == row(i + 2) && row(i + 2) == row(i + 3) && row(i) != "○") {
          return Some(row(i))
        }
      }
    }

    // Check for vertical wins
    for (col <- 0 to 6) {
      for (row <- 0 to 2) {
        if (board(row)(col) == board(row + 1)(col) && board(row + 1)(col) == board(row + 2)(col) && board(row + 2)(col) == board(row + 3)(col) && board(row)(col) != "○") {
          return Some(board(row)(col))
        }
      }
    }

    // Check for diagonal wins (top-left to bottom-right)
    for (row <- 0 to 2) {
      for (col <- 0 to 3) {
        if (board(row)(col) == board(row + 1)(col + 1) && board(row + 1)(col + 1) == board(row + 2)(col + 2) && board(row + 2)(col + 2) == board(row + 3)(col + 3) && board(row)(col) != "○") {
          return Some(board(row)(col))
        }
      }
    }

    // Check for diagonal wins (bottom-left to top-right)
    for (row <- 3 to 5) {
      for (col <- 0 to 3) {
        if (board(row)(col) == board(row - 1)(col + 1) && board(row - 1)(col + 1) == board(row - 2)(col + 2) && board(row - 2)(col + 2) == board(row - 3)(col + 3) && board(row)(col) != "○") {
          return Some(board(row)(col))
        }
      }
    }

    // Return None if no player has won
    None
  }
}