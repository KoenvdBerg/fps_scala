package aoc

object Blizzard {

  def rowSize = 8

  def possibleMoves(pos: Int): List[Int] = {
    def getMoves(pos: Int): List[Int] = {
      val left = if pos % this.rowSize != 0 then pos - 1 else -1
      val right = if pos + 1 % this.rowSize != 0 then pos + 1 else -1
      val up = pos - this.rowSize
      val down = pos + this.rowSize
      List(left, right, up, down).filter(_ >= 0)
    }
    getMoves(pos)
  }

    def shift(board: Array[(Int, Char)]): Array[(Int, Char)] = {
      def updaterc(pos: Int): Int = {
        if board(pos)._2 == '#' then pos - (this.rowSize - 2) else pos + 1
      }
      def updatelc(pos: Int): Int = {
        if board(pos)._2 == '#' then pos + (this.rowSize - 2) else pos - 1
      }

      board.map(x => x match
        case (i, '>') => (updaterc(i), '>')
        case (i, '<') => (updatelc(i), '<')
        case _ => x
      )

  }

  def parseBoard(bs: String): Array[(Int, Char)] = {
    val board = bs.split("\n").flatten.zipWithIndex.map((x,y) => (y,x))
    board
  }

  def printBoard(board: Array[(Int, Char)], rz: Int): Unit = {
    def go(n: Int, ss: String): Unit = {
      if n % rz == 0 then
        print(ss(n))
        println()
        go(n+1, ss)
      else if n >= ss.length then println()
      else
        print(ss(n))
        go(n+1, ss)
    }
    val x = board.map(_(1)).mkString
    go(0, x)
  }
}


@main def runBoardAnalysis(): Unit = {
  val boardstring = "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#"

  val board = Blizzard.parseBoard(bs=boardstring)

  println(Blizzard.possibleMoves(4))
  println(Blizzard.possibleMoves(5))
  println(Blizzard.possibleMoves(7))
  println(Blizzard.possibleMoves(18))
}