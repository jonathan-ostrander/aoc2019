package aoc

object Problem10 extends App {
  println(
    Some(
      scala.io.Source.fromResource("inputs/10.txt")
        .getLines
        .takeWhile(_ != "")
        .toList
        .zipWithIndex
        .flatMap(l =>
          l._1
            .zipWithIndex
            .flatMap(c => if (c._1 == '#') Some(l._2.toDouble -> c._2.toDouble) else None)
        )
    ).map(androids =>
      androids.map(a =>
        androids
          .map(b => b -> Math.atan2(b._1 - a._1, b._2 - a._2))
          .groupBy(_._2)
          .transform((_, t) => t.map(_._1).sortBy(v => Math.pow(v._1 - a._1, 2) + Math.pow(v._2 - a._2, 2)))
      ).maxBy(_.keySet.size)
    ).map(m =>
      List[Map[Double, List[(Double, Double)]] => Int](
        _.keySet.size,
        m =>
          Stream.iterate(
            Stream.iterate(m.toList.sortBy(_._1))(l =>
              if (l.head._1 < (-1*Math.PI/2)) l.tail :+ l.head
              else l
            ).dropWhile(_.head._1 < (-1*Math.PI/2))
            .head
          )(l => l.tail :+ (l.head._1 -> l.head._2.tail))
            .drop(199)
            .take(1)
            .map(l => l.head._2.head)
            .map(p => (p._2*100 + p._1).toInt)
            .head
      ).map(_(m))
    )
      .get
      .zipWithIndex
      .map(v => s"Part ${v._2 + 1}: ${v._1}")
      .mkString("\n")
  )
}
