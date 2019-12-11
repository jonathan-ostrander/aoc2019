package aoc

object Problem09 extends App {
  println(
    List(1L, 2L).map(c =>
      LazyList.continually(c).scanLeft(
        (
          scala.io.Source.fromResource("inputs/9.txt").getLines.next.split(",")
            .map(_.toLong)
            .zipWithIndex
            .map(t => t._2.toLong -> t._1)
            .toMap,
          0L,
          true,
          List.empty[Long],
          0L
        )
      )((a, input) =>
        Some("%05d".format(a._1(a._2)).zipWithIndex.map(_.swap).toMap).map(cm =>
          Some((m: Char) =>
            if (m == '0') ((i: Long) => a._1.getOrElse(a._1.getOrElse(i, 0L), 0L), (i: Long) => a._1.getOrElse(i, 0L))
            else if (m == '1') ((i: Long) => a._1.getOrElse(i, 0L), (i: Long) => i)
            else if (m == '2') ((i: Long) => a._1.getOrElse(a._5 + a._1.getOrElse(i, 0L), 0L), (i: Long) => a._5 + a._1.getOrElse(i, 0L))
            else sys.error("bad mode input")
          ).map(f => cm -> (0 to 2).map(i => f(cm(i)))).get
        ).map(t =>
          if (t._1(3) == '9' && t._1(4) == '9') (a._1, a._2, false, a._4, a._5)
          else if (t._1(4) == '1') (a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (t._2(2)._1(a._2 + 1L) + t._2(1)._1(a._2 + 2L))), a._2 + 4L, true, a._4, a._5)
          else if (t._1(4) == '2') (a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (t._2(2)._1(a._2 + 1L) * t._2(1)._1(a._2 + 2L))), a._2 + 4L, true, a._4, a._5)
          else if (t._1(4) == '3') (a._1 ++ Map(t._2(2)._2(a._2 + 1L) -> input), a._2 + 2L, true, a._4, a._5)
          else if (t._1(4) == '4') (a._1, a._2 + 2, true, t._2(2)._1(a._2 + 1L) :: a._4, a._5)
          else if (t._1(4) == '5') (a._1, if (t._2(2)._1(a._2 + 1L) != 0L) t._2(1)._1(a._2 + 2L) else a._2 + 3, true, a._4, a._5)
          else if (t._1(4) == '6') (a._1, if (t._2(2)._1(a._2 + 1L) == 0L) t._2(1)._1(a._2 + 2L) else a._2 + 3, true, a._4, a._5)
          else if (t._1(4) == '7') (a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (if (t._2(2)._1(a._2 + 1L) < t._2(1)._1(a._2 + 2L)) 1L else 0L)), a._2 + 4L, true, a._4, a._5)
          else if (t._1(4) == '8') (a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (if (t._2(2)._1(a._2 + 1L) == t._2(1)._1(a._2 + 2L)) 1L else 0L)), a._2 + 4L, true, a._4, a._5)
          else if (t._1(4) == '9') (a._1, a._2 + 2, a._3, a._4, a._5 + t._2(2)._1(a._2 + 1L))
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
