package aoc

object Problem16 extends App {
  println(
    Some(scala.io.Source.fromResource("inputs/16.txt").getLines.next.map(_.toString.toInt))
      .map(l => List(l, (0 until 10000).flatMap(_ => l)))
      .map(q =>
        List(
          (v: IndexedSeq[Int]) => LazyList.iterate(v)(l =>
            (0 until v.length).foldLeft[(List[Int], Int)]((Nil, 0))((prev, i) =>
              if (i > (v.length / 2)) (prev._1 :+ ((prev._2 - l(i - 1)).abs % 10), prev._2 - l(i - 1))
              else Some((i until v.length).map(j => (j, ((j + 1) / (i + 1)) % 4)).foldLeft(0)((a, j) =>
                if (j._2 == 0 || j._2 == 2) a
                else if (j._2 == 1) a + l(j._1)
                else a - l(j._1)
              )).map(d => (prev._1 :+ (d.abs % 10), d)).get
            )._1.toIndexedSeq
          )
            .drop(100)
            .head
            .take(8)
            .mkString,
          (v: IndexedSeq[Int]) => LazyList.iterate(v.drop(v.take(7).mkString.toInt))(l => {
            (0 until l.length).reverse.foldLeft[(List[Int], Int)]((Nil, 0))((prev, i) =>
              if (prev._1.isEmpty) (l(i) :: Nil, l(i))
              else ((prev._2 + l(i)) % 10 :: prev._1, prev._2 + l(i))
            )._1.toIndexedSeq
          })
            .drop(100)
            .head
            .take(8)
            .mkString
        ).zip(q).map(t => t._1(t._2))
      )
        .get
        .zipWithIndex
        .map(v => s"Part ${v._2 + 1}: ${v._1}")
        .mkString("\n")
  )
}
