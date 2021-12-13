package problems

import common.BaseProblem

object Problem07 extends BaseProblem(7) {

  val initialPositions = readInput().next().split(',').map(_.toInt)

  def gasToSpot(l: Seq[Int], target: Int, costFunction: (Int, Int) => Int) =
    l.map(costFunction(_, target)).sum

  def minimumCost(f: (Int, Int) => Int) =
    (initialPositions.min to initialPositions.max)
      .map(gasToSpot(initialPositions, _, f))
      .min

  def solutionA = minimumCost((a, b) => (a - b).abs)

  def solutionB = minimumCost((a, b) => (a - b).abs * ((a - b).abs + 1) / 2)
}
