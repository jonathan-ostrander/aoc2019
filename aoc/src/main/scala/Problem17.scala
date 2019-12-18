package aoc

object Problem17 extends App { 
  println(
    Some(
      (om: Map[Long, Long], il: List[Long]) =>
        LazyList.continually(()).scanLeft(
          (
            scala.io.Source.fromResource("inputs/17.txt").getLines.next.split(",")
              .map(_.toLong)
              .zipWithIndex
              .map(t => t._2.toLong -> t._1)
              .toMap.withDefaultValue(0L) ++ om,
            0L,
            true,
            List.empty[Long],
            0L,
            il
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
            if (t._1(3) == '9' && t._1(4) == '9') (a._1, a._2, false, a._4, a._5, a._6)
            else if (t._1(4) == '1') (a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (t._2(2)._1(a._2 + 1L) + t._2(1)._1(a._2 + 2L))), a._2 + 4L, true, a._4, a._5, a._6)
            else if (t._1(4) == '2') (a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (t._2(2)._1(a._2 + 1L) * t._2(1)._1(a._2 + 2L))), a._2 + 4L, true, a._4, a._5, a._6)
            else if (t._1(4) == '3') (a._1 ++ Map(t._2(2)._2(a._2 + 1L) -> a._6.head), a._2 + 2L, true, a._4, a._5, a._6.tail)
            else if (t._1(4) == '4') (a._1, a._2 + 2, true, t._2(2)._1(a._2 + 1L) :: a._4, a._5, a._6)
            else if (t._1(4) == '5') (a._1, if (t._2(2)._1(a._2 + 1L) != 0L) t._2(1)._1(a._2 + 2L) else a._2 + 3, true, a._4, a._5, a._6)
            else if (t._1(4) == '6') (a._1, if (t._2(2)._1(a._2 + 1L) == 0L) t._2(1)._1(a._2 + 2L) else a._2 + 3, true, a._4, a._5, a._6)
            else if (t._1(4) == '7') (a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (if (t._2(2)._1(a._2 + 1L) < t._2(1)._1(a._2 + 2L)) 1L else 0L)), a._2 + 4L, true, a._4, a._5, a._6)
            else if (t._1(4) == '8') (a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (if (t._2(2)._1(a._2 + 1L) == t._2(1)._1(a._2 + 2L)) 1L else 0L)), a._2 + 4L, true, a._4, a._5, a._6)
            else if (t._1(4) == '9') (a._1, a._2 + 2, a._3, a._4, a._5 + t._2(2)._1(a._2 + 1L), a._6)
            else sys.error(s"bad op code input ${t._1}")
          ).get
        )
          .dropWhile(_._3)
          .head
    ).flatMap(f =>
      Some(
        f(Map.empty, Nil)
          ._4
          .reverse
          .map(_.toChar)
          .foldLeft[(Map[(Int, Int), Char], (Int, Int))]((Map.empty, (0, 0)))((m, p) =>
            if (p == '\n') (m._1, (0, m._2._2 + 1))
            else (m._1 ++ Map(m._2 -> p), (m._2._1 + 1, m._2._2))
          )._1
      ).flatMap(m =>
          m.find(t => Set('^', '>', 'v', '<')(t._2)).map(start =>
            LazyList.iterate((start, List.empty[(Char, Int)], true))(c => {
              List(
                (1, 0, '<', '>', (i: Char) => if (i == '^') 'R' else 'L'),
                (-1, 0, '>', '<', (i: Char) => if (i == '^') 'L' else 'R'),
                (0, 1, '^', 'v', (i: Char) => if (i == '<') 'L' else 'R'),
                (0, -1, 'v', '^', (i: Char) => if (i == '<') 'R' else 'L'),
              )
                .filterNot(t => t._3 == c._1._2 || t._4 == c._1._2)
                .find(t => m.get((c._1._1._1 + t._1, c._1._1._2 + t._2)).contains('#'))
                .map(t => (
                  t,
                  LazyList.from(1)
                    .dropWhile(i => m.get((c._1._1._1 + t._1*i, c._1._1._2 + t._2*i)).contains('#'))
                    .head - 1
                )).map(t => (
                  ((c._1._1._1 + t._2*t._1._1, c._1._1._2 + t._2*t._1._2), t._1._4),
                  c._2 :+ (t._1._5(c._1._2), t._2),
                  true
                ))
                .getOrElse((c._1, c._2, false))
            })
              .dropWhile(_._3)
              .head
              ._2
              .map(p => p._1.toString + "," + p._2.toString)
          ).flatMap(l => 
            LazyList.from((1 to 10).flatMap(i => (1 to 10).map(j => (i, j)))).flatMap(u =>
              Some((
                l.take(u._1),
                LazyList.iterate(l.drop(u._1))(_.drop(u._1)).dropWhile(_.take(u._1) == l.take(u._1)).head.take(u._2)
              )).flatMap(t =>
                Some(
                  LazyList.iterate((u._1 + u._2, List.empty[List[String]]))(p =>
                    if (l.drop(p._1).take(u._1) == t._1) (p._1 + u._1, p._2)
                    else if (l.drop(p._1).take(u._2) == t._2) (p._1 + u._2, p._2)
                    else Some(
                      l.zipWithIndex
                        .drop(p._1)
                        .takeWhile(w => l.drop(w._2).take(u._1) != t._1 && l.drop(w._2).take(u._2) != t._2)
                    ).map(c => (p._1 + c.length, p._2 :+ c.map(_._1))).get
                  ).dropWhile(_._1 < l.length).head._2
                )
                  .map(cs =>
                    cs.flatMap(q =>
                      if (q.length > cs.map(_.length).min)
                        List(q.take(cs.map(_.length).min), q.drop(cs.map(_.length).min))
                      else List(q)
                    )
                  )
                  .flatMap(cs => cs.headOption.filter(h => cs.forall(_ == h)))
                  .map(c => (t._1, t._2, c))
              ).map(t =>
                LazyList.iterate((0, List.empty[Char]))(p =>
                  if (l.drop(p._1).take(t._1.length) == t._1) (p._1 + t._1.length, p._2 :+ 'A')
                  else if (l.drop(p._1).take(t._2.length) == t._2) (p._1 + t._2.length, p._2 :+ 'B')
                  else if (l.drop(p._1).take(t._3.length) == t._3) (p._1 + t._3.length, p._2 :+ 'C')
                  else sys.error("unreachable code path")
                )
                  .dropWhile(_._1 < l.length)
                  .head
                  ._2.mkString(",") :: List(t._1, t._2, t._3).map(la => la.mkString(","))
              )
            ).find(q => q.forall(_.length < 20)).map(_.mkString("\n") + "\nn\n")
          ).map(m -> _)
      ).map(w => List[Long](
        w._1.filter(p =>
          p._2 == '#' &&
          List((0, 1), (0, -1), (1, 0), (-1, 0))
            .forall(t => w._1.get((p._1._1 + t._1, p._1._2 + t._2)).contains('#'))
        ).map(p => p._1._1 * p._1._2).sum.toLong,
        f(Map(0L -> 2L), w._2.toList.map(_.toLong))._4.head
      ))
    )
      .get
      .zipWithIndex
      .map(v => s"Part ${v._2 + 1}: ${v._1}")
      .mkString("\n")
  )
}
