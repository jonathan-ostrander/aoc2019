package aoc

object Problem08 extends App {
  println(
    Some(scala.io.Source.fromResource("inputs/8.txt").getLines.next.toList.sliding(25*6, 25*6).toList).map(layers =>
      List(
        layers.map(l => l.filter(_ == '0').size -> (l.filter(_ == '1').size * l.filter(_ == '2').size)).minBy(_._1)._2,
        layers.reduceLeft((o, t) =>
          o.zip(t).map(p => if (p._1 == '2') p._2 else p._1)
        ).sliding(25, 25).map(_.mkString("")).mkString("\n")
      )
    ).get
      .zipWithIndex
      .map(v => s"Part ${v._2 + 1}:\n${v._1}")
      .mkString("\n")
  )
}
