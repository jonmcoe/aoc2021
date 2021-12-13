package problems

import common.BaseProblem

object Problem05 extends BaseProblem(5) {

  case class Segment(origin: (Int, Int), terminus: (Int, Int)) {
    def isOrthogonalToAxis =
      origin._1 == terminus._1 || origin._2 == terminus._2

    val rangeOfPoints: Set[(Int, Int)] = {
      if (origin._1 == terminus._1) {
        val max = List(origin._2, terminus._2).max
        val min = List(origin._2, terminus._2).min
        (min to max).map(y => (origin._1, y)).toSet
      } else if (origin._2 == terminus._2) {
        val max = List(origin._1, terminus._1).max
        val min = List(origin._1, terminus._1).min
        (min to max).map(x => (x, origin._2)).toSet
      } else {
        val maxX = List(origin._1, terminus._1).max
        val minX = List(origin._1, terminus._1).min
        val lefty = List(origin, terminus).find(_._1 == minX).get
        val slope = (terminus._2 - origin._2) / (terminus._1 - origin._1)
        (minX to maxX).map(x => (x, lefty._2 + slope * (x - minX))).toSet
      }
    }
  }
  object Segment {
    def apply(raw: String): Segment = {
      val Array(rawOrigin, rawTerminus) = raw.split(" -> ")
      val Array(originX, originY) = rawOrigin.split(',').map(_.toInt)
      val Array(terminusX, terminusY) = rawTerminus.split(',').map(_.toInt)
      Segment((originX, originY), (terminusX, terminusY))
    }
  }

  def allSegments = readInput().map(x => Segment(x))

  def getOverlaps(s: Iterator[Segment]): Int =
    s.map(_.rangeOfPoints)
      .toList
      .combinations(2)
      .flatMap { case Seq(a, b) => a.intersect(b) }
      .toSet
      .size

  def solutionA = getOverlaps(allSegments.filter(_.isOrthogonalToAxis))
  def solutionB = getOverlaps(allSegments)
}
