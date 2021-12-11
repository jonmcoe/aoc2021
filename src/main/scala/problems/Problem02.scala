package problems

import common.BaseProblem

object Problem02 extends BaseProblem(2) {

  case class State(horizontal: Int, depth: Int, aim: Int)

  val directions: List[(Int, Int)] =
    readInput().map(_.split(" ").toList).toList.map {
      case List("up", x)      => (0, x.toInt * -1)
      case List("down", x)    => (0, x.toInt)
      case List("forward", x) => (x.toInt, 0)
    }

  def sumTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) =
    (a._1 + b._1, a._2 + b._2)

  def calculateNewPositionAndAim(
      current: State,
      incoming: (Int, Int)
  ): State = {
    val newAim = current.aim + incoming._2
    State(
      current.horizontal + incoming._1,
      current.depth + (newAim * incoming._1),
      newAim
    )
  }

  def solutionA: Int = {
    val (horizontal, depth) = directions.foldLeft((0, 0))(sumTuples)
    horizontal * depth
  }
  def solutionB: Int = {
    val State(horizontal, depth, _) =
      directions.foldLeft(State(0, 0, 0))(calculateNewPositionAndAim)
    horizontal * depth
  }
}
