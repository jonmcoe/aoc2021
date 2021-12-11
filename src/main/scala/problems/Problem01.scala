package problems

import common.BaseProblem

object Problem01 extends BaseProblem(1) {

  val integers = readInput().map(_.toInt).toList

  def solutionA: Int =
    integers.sliding(2).map(t => if (t.head < t.tail.head) 1 else 0).sum
  def solutionB: Int =
    integers
      .sliding(3)
      .sliding(2)
      .toList
      .map {
        case Seq(earlier, later) => if (earlier.sum < later.sum) 1 else 0
      }
      .sum
}
