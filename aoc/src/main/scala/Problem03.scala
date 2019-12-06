package aoc

object Problem03 extends App {
  println(
    Some(
      scala.io.Source.fromResource("inputs/3.txt")
        .getLines
        .take(2)
        .map(l => l.split(",").map(w => w(0) -> w.substring(1).toInt).toList)
        .map(
          _.scanLeft(((0, 0), (0, 0), 0, false, 0, (0, 0)))((s, m) =>
            Map('R' -> (1, 0), 'L' -> (-1, 0), 'U' -> (0, 1), 'D' -> (0, -1))
              .get(m._1)
              .map(t => (
                s._2,
                (s._2._1 + t._1 * m._2, s._2._2 + t._2 * m._2),
                s._3 + m._2,
                t._1 == 0,
                if (t._1 == 0) s._2._1 else s._2._2,
                Some(
                  if (t._1 == 0) (s._2._2, s._2._2 + m._2 * t._2)
                  else (s._2._1, s._2._1 + m._2 * t._1)
                ).map(p => if (p._1 > p._2) p.swap else p).get
              ))
              .getOrElse(sys.error(s"invalid character ${m._1}"))
          ).drop(1)
        )
        .sliding(2)
        .map(l => l(0).flatMap(s => l(1).map(s -> _)))
        .next
        .filter(p => p._1._4 ^ p._2._4)
        .map(p => if (p._1._4) p else p.swap)
        .filter(p => p._1._5 >= p._2._6._1 && p._1._5 <= p._2._6._2 && p._2._5 >= p._1._6._1 && p._2._5 <= p._1._6._2)
        .map(p => (
          Math.abs(p._1._5) + Math.abs(p._2._5),
          p._1._3 + p._2._3 - Math.abs(p._1._5 - p._2._2._1) - Math.abs(p._2._5 - p._1._2._2)
        ))
    ).map(m =>
      List[(Int, Int) => Int]((v, _) => v, (_, v) => v)
        .map(f => m.map(v => f(v._1, v._2)).min)
        .zipWithIndex
        .map(v => s"Part ${v._2 + 1}: ${v._1}")
        .mkString("\n")
    ).get
  )
}
