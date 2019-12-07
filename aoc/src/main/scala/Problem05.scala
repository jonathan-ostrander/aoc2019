package aoc

object Problem05 extends App {
  println(
    List(1, 5).map(c =>
      LazyList.continually(c).scanLeft(
        (
          scala.io.Source.fromResource("inputs/5.txt").getLines.next.split(",")
            .map(_.toInt)
            .zipWithIndex
            .map(_.swap)
            .toMap,
          0,
          true,
          List.empty[Int]
        )
      )((a, input) =>
        Some("%05d".format(a._1(a._2)).zipWithIndex.map(_.swap).toMap).map(cm =>
          Some((m: Char) =>
            if (m == '0') (i: Int) => a._1(a._1(i))
            else if (m == '1') (i: Int) => a._1(i)
            else sys.error("bad mode input")
          ).map(f => cm(4) -> (0 to 2).map(i => f(cm(i)))).get
        ).map(t =>
          if (t._1 == '9') (a._1, a._2, false, a._4)
          else if (t._1 == '1') (a._1 ++ Map(a._1(a._2 + 3) -> (t._2(2)(a._2 + 1) + t._2(1)(a._2 + 2))), a._2 + 4, true, a._4)
          else if (t._1 == '2') (a._1 ++ Map(a._1(a._2 + 3) -> (t._2(2)(a._2 + 1) * t._2(1)(a._2 + 2))), a._2 + 4, true, a._4)
          else if (t._1 == '3') (a._1 ++ Map(a._1(a._2 + 1) -> input), a._2 + 2, true, a._4)
          else if (t._1 == '4') (a._1, a._2 + 2, true, t._2(2)(a._2 + 1) :: a._4)
          else if (t._1 == '5') (a._1, if (t._2(2)(a._2 + 1) != 0) t._2(1)(a._2 + 2) else a._2 + 3, true, a._4)
          else if (t._1 == '6') (a._1, if (t._2(2)(a._2 + 1) == 0) t._2(1)(a._2 + 2) else a._2 + 3, true, a._4)
          else if (t._1 == '7') (a._1 ++ Map(a._1(a._2 + 3) -> (if (t._2(2)(a._2 + 1) < t._2(1)(a._2 + 2)) 1 else 0)), a._2 + 4, true, a._4)
          else if (t._1 == '8') (a._1 ++ Map(a._1(a._2 + 3) -> (if (t._2(2)(a._2 + 1) == t._2(1)(a._2 + 2)) 1 else 0)), a._2 + 4, true, a._4)
          else sys.error(s"bad op code input ${t._1}")
        ).get
      )
        .dropWhile(_._3)
        .head
        ._4
        .head
    )
    .zipWithIndex
    .map(v => s"Part ${v._2 + 1}: ${v._1}")
    .mkString("\n")
  )
}
