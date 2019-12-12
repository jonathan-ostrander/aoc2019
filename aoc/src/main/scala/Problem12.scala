package aoc

object Problem12 extends App {
  println(
    LazyList.iterate(
      (
        scala.io.Source.fromResource("inputs/12.txt")
          .getLines
          .takeWhile(_ != "")
          .map(_.split(",").map(s => s.filter(c => c.isDigit || c == '-').toInt))
          .map(a => (a.toList, List(0, 0, 0)))
          .toList,
        List(Set.empty[List[(Int, Int)]], Set.empty[List[(Int, Int)]], Set.empty[List[(Int, Int)]]),
        List(Option.empty[Long], Option.empty[Long], Option.empty[Long]),
        0L
      )
    )(ms =>
      (
        ms._1.map(m =>
          m._1 -> ms._1.foldLeft(m._2)((d, n) => d.zip(m._1.zip(n._1)).map(t =>
            if (t._2._1 < t._2._2) t._1 + 1
            else if (t._2._1 > t._2._2) t._1 - 1
            else t._1
          ))
        ).map(m => m._1.zip(m._2).map(t => t._1 + t._2) -> m._2),
        ms._2.zipWithIndex.map(t => t._1 + ms._1.map(q => q._1(t._2) -> q._2(t._2))),
        ms._2.zipWithIndex.zip(ms._3).map(t => if(t._2.isEmpty && t._1._1(ms._1.map(q => q._1(t._1._2) -> q._2(t._1._2)))) Some(ms._4) else t._2),
        ms._4 + 1
      )
    )
      .flatMap(a =>
        if (a._4 == 1000) Some(a._1.map(t => t._1.map(Math.abs).sum * t._2.map(Math.abs).sum).sum)
        else if (a._3.forall(_.isDefined)) Some(
          a._3
            .flatten
            .reduce((x, y) => (x*y) / LazyList.iterate((x, y))(t => (t._2, t._1 % t._2)).dropWhile(_._2 > 0).head._1)
        )
        else None
      )
      .take(2)
      .zipWithIndex
      .map(v => s"Part ${v._2 + 1}: ${v._1}")
      .mkString("\n")
  )
}
