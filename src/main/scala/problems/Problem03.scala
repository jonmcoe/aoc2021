package problems

import common.BaseProblem

import scala.collection.MapView

object Problem03 extends BaseProblem(3) {

  val matrix =
    readInput().map(_.toCharArray.map(_.toInt - '0'.toInt)).toArray

  def powersOfTwo = Iterator.unfold(1) { x => Some((x, x * 2)) }

  def binarySeqToDecimal(l: Seq[Int]) =
    l.reverse.zip(powersOfTwo).map { case (bit, mag) => bit * mag }.sum

  def solutionA = {
    val gamma = matrix.transpose
      .map(
        _.groupBy(identity).view.mapValues(_.length).maxBy(_._2)._1
      )
    val epsilon = matrix.transpose
      .map(
        _.groupBy(identity).view.mapValues(_.length).minBy(_._2)._1
      )
    binarySeqToDecimal(gamma) * binarySeqToDecimal(epsilon)
  }

  def filterToChoice(
      remainingMatrix: Array[Array[Int]],
      selectRep: MapView[Int, Int] => Int,
      i: Int = 0
  ): Array[Int] = {
    remainingMatrix match {
      case Array(el) => el
      case _ => {
        val transposed = remainingMatrix.transpose
        val counted = transposed(i)
          .groupBy(identity)
          .view
          .mapValues(_.length)
        val rep = selectRep(counted)
        filterToChoice(remainingMatrix.filter(_(i) == rep), selectRep, i + 1)
      }
    }
  }

  def solutionB =
    binarySeqToDecimal(
      filterToChoice(matrix, c => if (c(0) > c(1)) 0 else 1)
    ) * binarySeqToDecimal(
      filterToChoice(matrix, c => if (c(0) > c(1)) 1 else 0)
    )
}
