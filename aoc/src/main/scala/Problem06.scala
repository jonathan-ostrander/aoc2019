package aoc

object Problem06 extends App {
  println(
    Some(
      scala.io.Source.fromResource("inputs/6.txt")
        .getLines
        .takeWhile(_ != "")
        .map(_.split(')'))
        .map(a => a(1) -> Some(a(0)))
        .toMap ++ Map("COM" -> None)
    )
      .map(o => o.transform((k, v) => (v, o.filter(_._2.contains(k)).map(_._1).toList)))
      .map(o =>
        List(
          LazyList.iterate((List("COM" -> 0), 0))(l => 
            l._1.headOption.map(t =>
              (l._1.tail ++ o.getOrElse(t._1, (None, Nil))._2.map(_ -> (t._2 + 1)), l._2 + t._2)
            ).get
          )
            .dropWhile(_._1.nonEmpty)
            .head
            ._2,
          -2 + LazyList.iterate((List("YOU" -> 0), Set.empty[String]))(l =>
            l._1.headOption.flatMap(t =>
              o.get(t._1).map(p =>
                (l._1.tail ++ (p._1.toList ++ p._2).filterNot(l._2).map(_ -> (t._2 + 1)), l._2 + t._1)
              )
            ).getOrElse(l._1.tail -> l._2)
          )
            .dropWhile(_._1.head._1 != "SAN")
            .head
            ._1.head._2
        )
      )
      .get
      .zipWithIndex
      .map(v => s"Part ${v._2 + 1}: ${v._1}")
      .mkString("\n")
  )
}
