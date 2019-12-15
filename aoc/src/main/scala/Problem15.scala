package aoc
import scala.util.Random

object Problem15 extends App {
  println(
    LazyList.iterate(
      (
        (1 to 4).toList.map(i => (
          scala.io.Source.fromResource("inputs/15.txt").getLines.next.split(",")
            .map(_.toLong)
            .zipWithIndex
            .map(t => t._2.toLong -> t._1)
            .toMap.withDefaultValue(0L),
          0L,
          true,
          List.empty[Long],
          0L,
          ((0, 0), i)
        )),
        Map((0, 0) -> 1)
      )
    )(b =>
      b._1.headOption.map(a =>
        Some("%05d".format(a._1(a._2)).zipWithIndex.map(_.swap).toMap).map(cm =>
          Some((m: Char) =>
            if (m == '0') ((i: Long) => a._1(a._1(i)), (i: Long) => a._1(i))
            else if (m == '1') ((i: Long) => a._1(i), (i: Long) => i)
            else if (m == '2') ((i: Long) => a._1(a._5 + a._1(i)), (i: Long) => a._5 + a._1(i))
            else sys.error("bad mode input")
          ).map(f => cm -> (0 to 2).map(i => f(cm(i)))).get
        ).map(t =>
          if (t._1(3) == '9' && t._1(4) == '9') ((a._1, a._2, false, a._4, a._5, a._6) :: b._1.tail, b._2)
          else if (t._1(4) == '1') ((a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (t._2(2)._1(a._2 + 1L) + t._2(1)._1(a._2 + 2L))), a._2 + 4L, true, a._4, a._5, a._6) :: b._1.tail, b._2)
          else if (t._1(4) == '2') ((a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (t._2(2)._1(a._2 + 1L) * t._2(1)._1(a._2 + 2L))), a._2 + 4L, true, a._4, a._5, a._6) :: b._1.tail, b._2)
          else if (t._1(4) == '3') ((a._1 ++ Map(t._2(2)._2(a._2 + 1L) -> a._6._2.toLong), a._2 + 2L, true, a._4, a._5, a._6) :: b._1.tail, b._2)
          else if (t._1(4) == '4')
            Some((
              t._2(2)._1(a._2 + 1L),
              if (a._6._2 == 1L) (0, 1)
              else if (a._6._2 == 2L) (0, -1)
              else if (a._6._2 == 3L) (-1, 0)
              else (1, 0)
            )).map(o =>
              if (o._1 == 0L) (b._1.tail, b._2 ++ Map(((a._6._1._1 + o._2._1, a._6._1._2 + o._2._2), 0)))
              else (
                List((0, 1), (0, -1), (-1, 0), (1, 0))
                  .zipWithIndex
                  .map(i => ((a._6._1._1 + o._2._1, a._6._1._2 + o._2._2), i._1, (i._2 + 1)))
                  .filterNot(p => b._2.contains((p._1._1 + p._2._1, p._1._2 + p._2._2)))
                  .map(p => (a._1, a._2 + 2, a._3, o._1 :: a._4, a._5, (p._1, p._3))) ++ b._1.tail,
                b._2 ++ Map(((a._6._1._1 + o._2._1, a._6._1._2 + o._2._2), o._1.toInt))
              )
            ).get
          else if (t._1(4) == '5') ((a._1, if (t._2(2)._1(a._2 + 1L) != 0L) t._2(1)._1(a._2 + 2L) else a._2 + 3, true, a._4, a._5, a._6) :: b._1.tail, b._2)
          else if (t._1(4) == '6') ((a._1, if (t._2(2)._1(a._2 + 1L) == 0L) t._2(1)._1(a._2 + 2L) else a._2 + 3, true, a._4, a._5, a._6) :: b._1.tail, b._2)
          else if (t._1(4) == '7') ((a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (if (t._2(2)._1(a._2 + 1L) < t._2(1)._1(a._2 + 2L)) 1L else 0L)), a._2 + 4L, true, a._4, a._5, a._6) :: b._1.tail, b._2)
          else if (t._1(4) == '8') ((a._1 ++ Map(t._2(0)._2(a._2 + 3L) -> (if (t._2(2)._1(a._2 + 1L) == t._2(1)._1(a._2 + 2L)) 1L else 0L)), a._2 + 4L, true, a._4, a._5, a._6) :: b._1.tail, b._2)
          else if (t._1(4) == '9') ((a._1, a._2 + 2, a._3, a._4, a._5 + t._2(2)._1(a._2 + 1L), a._6) :: b._1.tail, b._2)
          else sys.error(s"bad op code input ${t._1}")
        ).get
      ).get
    )
      .dropWhile(_._1.nonEmpty)
      .headOption
      .map(_._2)
      .map(m => {
        // println(m)
        List(
          LazyList.iterate((List(((0, 0), 0)), Set((0, 0))))(l =>
            l._1.headOption.map(p =>
              List((1, 0), (-1, 0), (0, 1), (0, -1))
                .map(d => ((p._1._1 + d._1, p._1._2 + d._2), p._2 + 1))
                .filterNot(d => l._2(d._1) || m.getOrElse(d._1, 0) == 0)
            )
              .map(n => (l._1.tail ++ n, l._2 ++ n.map(_._1).toSet))
              .get
          )
            .dropWhile(l => !l._1.headOption.exists(p => m.getOrElse(p._1, 1) == 2))
            .head
            ._1
            .head
            ._2,
          LazyList.iterate((List((m.filter(_._2 == 2).head._1, 0)), Set((m.filter(_._2 == 2).head._1)), 0))(l =>
            l._1.headOption.map(p =>
              List((1, 0), (-1, 0), (0, 1), (0, -1))
                .map(d => ((p._1._1 + d._1, p._1._2 + d._2), p._2 + 1))
                .filterNot(d => l._2(d._1) || m.getOrElse(d._1, 1) == 0)
            )
              .map(n => (l._1.tail ++ n, l._2 ++ n.map(_._1).toSet, (l._3 :: n.map(_._2)).max))
              .get
          )
            .dropWhile(_._1.nonEmpty)
            .head
            ._3
        )
      })
        .get
        .zipWithIndex
        .map(v => s"Part ${v._2 + 1}: ${v._1}")
        .mkString("\n")
  )
}
