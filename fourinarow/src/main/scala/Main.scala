import scala.:+
import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import scala.util.control.Breaks._

object Main {
  def main(args: Array[String]): Unit = {
    var player: Int = 1
    var j: Int = 1
    var player1History: Array[Int] = Array()
    var player2History: Array[Int] = Array()

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


    breakable {
      while (true) {
        // Errorhandling for out of bounds
        var continue: Boolean = false
        var input: Int = -1
        while(continue == false){
          print("Now Playing: " + player + " Enter column: ")
          input = readInt

          if (input > cols || input < 1) {
            println("The number you chose is not valid!")
          } else {
            continue = true
          }
        }

        var selectedCol: Int = input

        //check current player, switch if neccessary and add to history
        if (player == 1) {
          player1History = player1History :+ selectedCol;
          player = 2
        } else {
          player2History = player2History :+ selectedCol;
          player = 1
        }

        //makes sure that when an element is dropped it doesn't overwrite but goes on top
        for (i <- rows to 1 by -1) {
          if (gameBoard(rows - i)(selectedCol - 1) == "○") {
            j = i
          }
        }

        drop(player, selectedCol, rows - j, cols, gameBoard)

        //print gameboard and playerhistroy
        val gameBoardtoString = gameBoard.map(_.mkString(" ")).mkString("\n")
        println(gameBoardtoString)

        println("Player 1: " + player1History.mkString(","))
        println("Player 2: " + player2History.mkString(","))

        //checks for wins and if there is one prints accordingly and ends the main game loop
        if (check(gameBoard).contains("◍")) {
          println("Player 1 won")
          break
        } else if (check(gameBoard).contains("⬤")) {
          println("Player 2 won")
          break
        }
      }
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
      for (i <- 0 to row.length - 4) {
        if (row(i) == row(i + 1) && row(i + 1) == row(i + 2) && row(i + 2) == row(i + 3) && row(i) != "○") {
          return Some(row(i))
        }
      }
    }

    // Check for vertical wins
    for (col <- 0 until board(0).length) {
      for (row <- 0 to board.length - 4) {
        if (board(row)(col) == board(row + 1)(col) && board(row + 1)(col) == board(row + 2)(col) && board(row + 2)(col) == board(row + 3)(col) && board(row)(col) != "○") {
          return Some(board(row)(col))
        }
      }
    }

    // Check for diagonal wins (top-left to bottom-right)
    for (row <- 0 to board.length - 4) {
      for (col <- 0 to board(0).length - 4) {
        if (board(row)(col) == board(row + 1)(col + 1) && board(row + 1)(col + 1) == board(row + 2)(col + 2) && board(row + 2)(col + 2) == board(row + 3)(col + 3) && board(row)(col) != "○") {
          return Some(board(row)(col))
        }
      }
    }

    // Check for diagonal wins (bottom-left to top-right)
    for (row <- board.length - 1 to 3 by -1) {
      for (col <- 0 to board(0).length - 4) {
        if (board(row)(col) == board(row - 1)(col + 1) && board(row - 1)(col + 1) == board(row - 2)(col + 2) && board(row - 2)(col + 2) == board(row - 3)(col + 3) && board(row)(col) != "○") {
          return Some(board(row)(col))
        }
      }
    }

    // Return None if no player has won
    None
  }
}
