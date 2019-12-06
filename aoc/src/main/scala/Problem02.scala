  package aoc

  object Problem02 extends App {
    println(
      (Vector(Vector(12, 2)) ++ ((0 to 100) ++ (0 to 100)).combinations(2)).map(v =>
        (
          v,
          Stream.from(0, 4).scanLeft(
            (
              scala.io.Source.fromResource("inputs/2.txt")
                .getLines
                .next
                .split(",")
                .map(_.toInt)
                .zipWithIndex
                .map(_.swap)
                .toMap ++ Map(1 -> v(0), 2 -> v(1)),
              true
            )
          )((a, i) =>
            if (a._1(i) == 99) a._1 -> false
            else if (a._1(i) == 1) a._1 ++ Map(a._1(i + 3) -> (a._1(a._1(i + 1)) + a._1(a._1(i + 2)))) -> true
            else if (a._1(i) == 2) a._1 ++ Map(a._1(i + 3) -> (a._1(a._1(i + 1)) * a._1(a._1(i + 2)))) -> true
            else sys.error("bad input")
          )
            .dropWhile(_._2)
            .head
            ._1(0)
        )
      )
        .zipWithIndex
        .flatMap(t =>
          if (t._2 == 0) Some(s"Part 1: ${t._1._2}")
          else if (t._1._2 == 19690720) Some(s"Part 2: ${100 * t._1._1(0) + t._1._1(1)}")
          else None
        )
        .take(2)
        .mkString("\n")
    )
  }
