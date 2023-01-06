import scala.:+
import scala.io.StdIn.readLine
import scala.io.Source
import scala.io.StdIn.readInt
import scala.util.{Try, Success, Failure}
import scala.util.control.Breaks._
import java.io.PrintWriter
import java.io._
 
object Main {
  def main(args: Array[String]): Unit = {
    //Menu at game start
    println("Please pick one of the following options by entering number: ")
    println("1. New Game ")
    println("2. Load Game ")
    println("0. Exit ")
 
    var x: Int = readInt
 
    x match {
      case 1 => newGame()
      case 2 => load()
      case 0 => System.exit(0)
      case _ => println("The number you entered does not correspond to any menu options")
    }
  }
 
  def newGame(
               gameBoard: Option[Array[Array[String]]] = None,
               player1History: Option[Array[Int]] = None,
               player2History: Option[Array[Int]] = None,
               playerLoaded: Int = 3
             ): Unit = {
    var j: Int = 1
    var player: Int = -1
    var rows: Int = 6
    var cols: Int = 7
    var checkSize: Boolean = false
 
    //makes sure that the board is within limits
    breakable{
      while(checkSize == false){
        rows = gameBoard.map(_.length).getOrElse {
          print("Enter number of rows: ")
          readInt
        }
        cols = gameBoard.map(_(0).length).getOrElse {
          print("Enter number of columns: ")
          readInt
        }
 
        if (rows<6 || cols < 7) {
          println("The minimum allowed dimensions for the board are 6x7")
        } else {
          if (rows - cols >= 2 || rows - cols <= -3) {
            println("There may be no more than a 2 difference between rows and columns")
          } else {
            checkSize = true
          }
        }
      }
    }
 
    //creates the board
    val gameBoardArray = gameBoard.getOrElse {
      val gameBoardArray = Array.ofDim[String](rows, cols)
      for (i <- gameBoardArray.indices) {
        for (j <- gameBoardArray(i).indices) {
          if (gameBoardArray(i)(j) == null) {
            gameBoardArray(i)(j) = "○"
          }
        }
      }
      gameBoardArray
    }
 
    //used for checking which players turn it is
    if(playerLoaded == 3) {
      player = 1;
    } else {
      player = playerLoaded
      println(gameBoardArray.map(_.mkString(" ")).mkString("\n"))
    }
 
    //player history
    var player1HistoryArray = player1History.getOrElse(Array())
    var player2HistoryArray = player2History.getOrElse(Array())
    println("Type any letter to save")
 
    //game loop
    breakable {
      while (true) {
        // Errorhandling for out of bounds
        var continue: Boolean = false
        var input: String = "-1"
        var inputInt: Int = -1
        var turnNumber: Int = player1HistoryArray.length + player2HistoryArray.length;
        var boardSize: Int = rows * cols
        while (continue == false) {
 
          //check for draws
          if(turnNumber >= boardSize) {
            println("The game is a draw!")
            break
          }
 
          print("Now Playing: " + player + " Enter column: ")
          input = readLine
 
          val result: Try[Int] = Try(input.toInt)
 
          result match {
            case Success(n) => inputInt = input.toInt
            case Failure(_) => save(gameBoardArray, player1HistoryArray, player2HistoryArray, player)
          }
 
          if (inputInt > cols || inputInt < 1) {
            println("The number you chose is not valid!")
 
          } else {
            continue = true
          }
        }
 
        var selectedCol: Int = inputInt
 
        //check current player, switch if neccessary and add to history
        if (player == 1) {
          player1HistoryArray = player1HistoryArray :+ selectedCol;
          player = 2
        } else {
          player2HistoryArray = player2HistoryArray :+ selectedCol;
          player = 1
        }
 
        //makes sure that when an element is dropped it doesn't overwrite but goes on top
        for (i <- rows to 1 by -1) {
          if (gameBoardArray(rows - i)(selectedCol - 1) == "○") {
            j = i
          }
        }
 
        drop(player, selectedCol, rows - j, cols, gameBoardArray)
 
        //print gameboard and playerhistroy
        val gameBoardtoString = gameBoardArray.map(_.mkString(" ")).mkString("\n")
        println(gameBoardtoString)
 
        println("Player 1: " + player1HistoryArray.mkString(","))
        println("Player 2: " + player2HistoryArray.mkString(","))
 
        //checks for wins and if there is one prints accordingly and ends the main game loop
        if (check(gameBoardArray).contains("◍")) {
          println("Player 1 won")
          break
        } else if (check(gameBoardArray).contains("⬤")) {
          println("Player 2 won")
          break
        }
      }
    }
  }
 
  //drop a disc into a column
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
 
  //win checking logic
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
 
  //save current game state
  def save(data: Array[Array[String]], moveHistory1: Array[Int], moveHistory2: Array[Int], player: Int): Unit = {
 
    val board = new PrintWriter(new File("board.txt"))
    val moveHistory = new PrintWriter(new File("moveHistory.txt"))
    val playerFile = new PrintWriter(new File("player.txt"))
    //save board state
    data.foreach(x => board.write(x.mkString(",") + "\n"))
    board.close
    //save history state
    moveHistory1.foreach(z => moveHistory.write(z.toString + ","))
    moveHistory.write("\n")
    moveHistory2.foreach(y => moveHistory.write(y.toString + ","))
    moveHistory.close
    //save current player state
    playerFile.write(player.toString)
    playerFile.close
  }
 
  def load(): Unit = {
    //load board state
    val boardFile = Source.fromFile("board.txt")
    val boardLines = boardFile.getLines.toArray
    val boardData = boardLines.map(_.split(","))
    boardFile.close
    //load history state
    val moveHistoryFile = Source.fromFile("moveHistory.txt")
    val moveHistoryLines = moveHistoryFile.getLines.toArray
    val moveHistory1 = moveHistoryLines(0).split(",").map(_.toInt)
    val moveHistory2 = moveHistoryLines(1).split(",").map(_.toInt)
    moveHistoryFile.close
    //load current player state
    val playerFile = Source.fromFile("player.txt")
    val player = playerFile.getLines.next.toInt
    playerFile.close
 
    //load game from last save
    newGame(Some(boardData), Some(moveHistory1), Some(moveHistory2), player)
  }
}
