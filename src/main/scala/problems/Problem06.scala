package problems

import common.BaseProblem

object Problem06 extends BaseProblem(6) {

  val initial =
    readInput()
      .next()
      .split(',')
      .map(_.toInt)
      .groupBy(identity)
      .view
      .mapValues(_.length.toLong)
      .toMap

  def step(m: Map[Int, Long]): Map[Int, Long] = {
    val numNew = m.getOrElse(0, 0L)
    val existing = m.map {
      case (t, freq) => if (t == 0) (6, freq) else (t - 1, freq)
    } // TODO: possible to handle the merging case successfully?
    existing + (8 -> numNew) + (6 -> (numNew + m.getOrElse(7, 0L)))
  }

  def applyRepeatedly[A](l: A, f: A => A, n: Int): A = {
    (1 to n).foldLeft(l) { case (prev, _) => f(prev) }
  }

  def solutionA = applyRepeatedly(initial, step, 80).values.sum

  def solutionB = applyRepeatedly(initial, step, 256).values.sum
}
