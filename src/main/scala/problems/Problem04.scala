package problems

import common.BaseProblem

object Problem04 extends BaseProblem(4) {

  case class Board(squares: Array[Array[Int]]) {

    def isFinished(called: Seq[Int]): Boolean = {
      val calledSet = called.toSet
      squares.exists(_.forall(calledSet.contains)) || squares.transpose.exists(
        _.forall(calledSet.contains)
      )
    }

    def score(called: Seq[Int]): Option[Int] = {
      if (isFinished(called)) {
        val unmarked =
          squares.flatten.filter(!called.contains(_))
        Some(called.last * unmarked.sum)
      } else None
    }
  }

  def parse: (Seq[Int], Seq[Board]) = {
    val lines = readInput().toSeq
    val fullChosen = lines.head.split(',').map(_.toInt)
    val boardBlocks = lines
      .drop(2)
      .grouped(6)
      .map(_.take(5))

    val boards = boardBlocks
      .map {
        _.map(_.split(' ').flatMap(_.toIntOption)).toArray
      }
      .map(Board)
    (fullChosen, boards.toSeq)
  }

  def solutionA: Int = {
    val (chosen, boards) = parse
    (1 to chosen.length).foreach { x =>
      boards.foreach { b =>
        val attempt = b.score(chosen.take(x))
        if (attempt.isDefined) {
          return attempt.get
        }
      }
    }
    -1
  }

  def solutionB = {
    val (chosen, boards) = parse
    var last: Option[Int] = Some(-1)
    (1 to chosen.length).foreach { x =>
      boards.foreach { b =>
        val attempt = b.score(chosen.take(x))
        if (attempt.isDefined) {
          last = attempt
          b.squares(1) = Array(-1, -1, -1, -1, -1)
          b.squares(2) = Array(-1, -1, -1, -1, -1)
          b.squares(3) = Array(-1, -1, -1, -1, -1)
          b.squares(4) = Array(-1, -1, -1, -1, -1)
          b.squares(0) = Array(-1, -1, -1, -1, -1)

        }
      }
    }
    last.get
  }

}
