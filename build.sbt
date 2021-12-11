name := "aoc2021"

version := "0.1"

scalaVersion := "2.13.7"

commands += Command("p") { _ =>
  import complete.DefaultParsers._
  (' ' ~ charClass(_.isDigit, "digit").+.map(_.mkString.toInt)).map(_._2)
} {
  case (previousState, i: Int) =>
    Command.process(
      "runMain problems.Problem" + "%02d".format(i),
      previousState
    )
}

// TODO: disable warning for unexhaustive pattern match, "multiple main classes detected"
