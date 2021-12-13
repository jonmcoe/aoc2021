package problems

import common.BaseProblem

object Problem06 extends BaseProblem(6) {

  val initial = readInput().next().split(',').map(_.toInt).toSeq

  def step(l: Seq[Int]): Seq[Int] = {
    val numNew = l.count(_ == 0)
    val existing = l.map(x => if (x == 0) 6 else x - 1)
    existing ++ List.fill(numNew)(8)
  }

  def applyRepeatedly[A](l: A, f: A => A, n: Int): A = {
    (1 to n).foldLeft(l) { case (prev, _) => f(prev) }
  }

  def solutionA = applyRepeatedly(initial, step, 80).length

  def solutionB = applyRepeatedly(initial, step, 256).length // OOM
}
