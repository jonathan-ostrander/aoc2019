package aoc

object Problem07 extends App {
  println(
    Some(
      scala.io.Source.fromResource("inputs/7.txt").getLines.next.split(",")
        .map(_.toInt)
        .zipWithIndex
        .map(_.swap)
        .toMap
    ).map(comp =>
      List((0 to 4).permutations, (5 to 9).permutations).map(_.map(v =>
        LazyList.iterate(
          (0, v.zipWithIndex.map(i => (comp, 0, true, true, List.empty[Int], if (i._2 == 0) -2 else -1)))
        )(q =>
          (
            (q._1 + 1) % 5,
            q._2.take(q._1) ++ IndexedSeq(LazyList.iterate((q._2(q._1)._1, q._2(q._1)._2, true, q._2(q._1)._4, q._2(q._1)._5, q._2(q._1)._6))(a =>
              Some("%05d".format(a._1(a._2)).zipWithIndex.map(_.swap).toMap).map(cm =>
                Some((m: Char) =>
                  if (m == '0') (i: Int) => a._1(a._1(i))
                  else if (m == '1') (i: Int) => a._1(i)
                  else sys.error("bad mode input")
                ).map(f => cm(4) -> (0 to 2).map(i => f(cm(i)))).get
              ).map(t =>
                if (t._1 == '9') (a._1, a._2, false, false, a._5, a._6)
                else if (t._1 == '1') (a._1 ++ Map(a._1(a._2 + 3) -> (t._2(2)(a._2 + 1) + t._2(1)(a._2 + 2))), a._2 + 4, true, true, a._5, a._6)
                else if (t._1 == '2') (a._1 ++ Map(a._1(a._2 + 3) -> (t._2(2)(a._2 + 1) * t._2(1)(a._2 + 2))), a._2 + 4, true, true, a._5, a._6)
                else if (t._1 == '3' && q._1 == 0 && a._6 == -1) (a._1 ++ Map(a._1(a._2 + 1) -> 0), a._2 + 2, true, true, a._5, a._6 + 1)
                else if (t._1 == '3' && a._6 < 0) (a._1 ++ Map(a._1(a._2 + 1) -> v(q._1)), a._2 + 2, true, true, a._5, a._6 + 1)
                else if (t._1 == '3') q._2((q._1 + 4) % 5)._5.drop(a._6).headOption.map(h =>
                  (a._1 ++ Map(a._1(a._2 + 1) -> h), a._2 + 2, true, true, a._5, a._6 + 1)
                ).getOrElse((a._1, a._2, false, true, a._5, a._6))
                else if (t._1 == '4') (a._1, a._2 + 2, true, true, a._5 :+ t._2(2)(a._2 + 1), a._6)
                else if (t._1 == '5') (a._1, if (t._2(2)(a._2 + 1) != 0) t._2(1)(a._2 + 2) else a._2 + 3, true, true, a._5, a._6)
                else if (t._1 == '6') (a._1, if (t._2(2)(a._2 + 1) == 0) t._2(1)(a._2 + 2) else a._2 + 3, true, true, a._5, a._6)
                else if (t._1 == '7') (a._1 ++ Map(a._1(a._2 + 3) -> (if (t._2(2)(a._2 + 1) < t._2(1)(a._2 + 2)) 1 else 0)), a._2 + 4, true, true, a._5, a._6)
                else if (t._1 == '8') (a._1 ++ Map(a._1(a._2 + 3) -> (if (t._2(2)(a._2 + 1) == t._2(1)(a._2 + 2)) 1 else 0)), a._2 + 4, true, true, a._5, a._6)
                else sys.error(s"bad op code input ${t._1}, ${a._2}, ${v}")
              ).get
            )
              .dropWhile(_._3)
              .head) ++ q._2.drop(q._1).tail
          )
        ).dropWhile(q => q._2.exists(_._4)).head._2(4)._5.last
      ).max)
    ).get
    .zipWithIndex
    .map(v => s"Part ${v._2 + 1}: ${v._1}")
    .mkString("\n")
  )
}
