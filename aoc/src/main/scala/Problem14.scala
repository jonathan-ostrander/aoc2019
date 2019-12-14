package aoc

object Problem14 extends App {
  println(
    Some(
      scala.io.Source.fromResource("inputs/14.txt")
        .getLines
        .takeWhile(_ != "")
        .map(_.split("=>"))
        .map(a => a(1).trim.split(" ") -> a(0).trim.split(", ").map(_.split(" ")))
        .map(t => t._1(1) -> (t._1(0).toLong -> t._2.toList.map(p => p(1) -> p(0).toLong)))
        .toMap
    ).map(m =>
      (f: Long) => LazyList.iterate((
        List(("FUEL", f)),
        Map.empty[String, Long].withDefaultValue(0L),
        0L
      ))(v =>
        v._1.headOption.map(t =>
          if (v._2(t._1) > t._2) (v._1.tail, v._2 + (t._1 -> (v._2(t._1) - t._2)), v._3)
          else if (v._2(t._1) == t._2) (v._1.tail, v._2 - t._1, v._3)
          else if (v._2(t._1) > 0L) (v._1.tail :+ (t._1, t._2 - v._2(t._1)), v._2 - t._1, v._3)
          else Some(
            if (t._2 % m(t._1)._1 == 0L) (t._2 / m(t._1)._1, 0L)
            else (t._2 / m(t._1)._1 + 1L, m(t._1)._1 - (t._2 % m(t._1)._1))
          ).map(u =>
            (m(t._1)._2.map(q => (q._1, q._2*u._1)), u._2)
          ).map(u =>
            (
              v._1.tail ++ u._1.filterNot(_._1 == "ORE"),
              v._2 + (t._1 -> u._2),
              v._3 + u._1.filter(_._1 == "ORE").map(_._2).sum
            )
          ).get
        ).get
      )
        .dropWhile(_._1.nonEmpty)
        .head
        ._3
    ).map(f => List(
      f(1),
      LazyList.iterate((0L, 1000000000000L))(t =>
        Some(f((t._2 + t._1)/ 2)).map(e =>
          if (e < 1000000000000L) ((t._2 + t._1)/ 2, t._2)
          else if (e == 1000000000000L) ((t._2 + t._1)/ 2, ((t._2 + t._1)/ 2) + 1)
          else (t._1, (t._2 + t._1)/ 2)
        ).get
      )
        .dropWhile(t => (t._2 - t._1) > 1)
        .head
        ._1
    ))
      .get
      .zipWithIndex
      .map(v => s"Part ${v._2 + 1}: ${v._1}")
      .mkString("\n")
  )
}
