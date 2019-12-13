package aoc

object Problem13 extends App {
  println(
    List(
      (Map.empty[Long, Long], (m: Map[(Int, Int), Int]) => m.toList.filter(_._2 == 2).length),
      (Map(0L -> 2L), (m: Map[(Int, Int), Int]) => m((-1, 0))),
    ).map(e =>
      e._2(
        LazyList.continually(0L).scanLeft(
          (
            scala.io.Source.fromResource("inputs/13.txt").getLines.next.split(",")
              .map(_.toLong)
              .zipWithIndex
              .map(t => t._2.toLong -> t._1)
              .toMap.withDefaultValue(0L) ++ e._1,
            0L,
            true,
            List.empty[Long],
            0L
          )
        )((a, _) =>
          Some("%05d".format(a._1(a._2)).zipWithIndex.map(_.swap).toMap).map(cm =>
            Some((m: Char) =>
              if (m == '0') ((i: Long) => a._1(a._1(i)), (i: Long) => a._1(i))
              else if (m == '1') ((i: Long) => a._1(i), (i: Long) => i)
              else if (m == '2') ((i: Long) => a._1(a._5 + a._1(i)), (i: Long) => a._5 + a._1(i))
              else sys.error("bad mode input")
            ).map(f => cm -> (0 to 2).map(i => f(cm(i)))).get
          ).map(t =>
            if (t._1(3) == '9' && t._1(4) == '9') (a._1, a._2, false, a._4, a._5)
            else if (t._1(4) == '1') (a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (t._2(2)._1(a._2 + 1L) + t._2(1)._1(a._2 + 2L))), a._2 + 4L, true, a._4, a._5)
            else if (t._1(4) == '2') (a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (t._2(2)._1(a._2 + 1L) * t._2(1)._1(a._2 + 2L))), a._2 + 4L, true, a._4, a._5)
            else if (t._1(4) == '3') 
              Some(
                a._4.reverse.sliding(3, 3).foldLeft[Map[(Long, Long), Long]](Map.empty)((m, l) => m ++ Map((l(0), l(1)) -> l(2))).filter(m => m._2 >= 0L && m._2 <= 4L)
              )
                .flatMap(m => m.find(_._2 == 3).flatMap(p => m.find(_._2 == 4).map(b => if (b._1._1 < p._1._1) -1L else if (b._1._1 > p._1._1) 1L else 0L)))
                .map(input => (a._1 ++ Map(t._2(2)._2(a._2 + 1L) -> input), a._2 + 2L, true, a._4, a._5)).get
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
          .reverse
          .sliding(3, 3)
          .map(l => (l(0).toInt, l(1).toInt) -> l(2).toInt)
          .toMap
      )
    )
    .zipWithIndex
    .map(v => s"Part ${v._2 + 1}: ${v._1}")
    .mkString("\n")
  )
}
