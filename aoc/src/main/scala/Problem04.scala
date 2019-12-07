package aoc

object Problem04 extends App {
  println(
    Some(scala.io.Source.fromResource("inputs/4.txt").getLines.next.split("-").map(_.toInt))
      .map(a =>
        (a(0) to a(1))
          .map(_.toString.toList.sliding(2).toList)
          .filter(s => s.exists(p => p(0) == p(1)) && s.forall(p => p(0) <= p(1)))
          .foldLeft(List(0, 0))((l, s) =>
            l(0) + 1 ::
            l(1) + (if (s.filter(p => p(0) == p(1)).exists(p => s.filter(_ == p).length == 1)) 1 else 0) ::
            Nil
          )
      )
      .get
      .zipWithIndex
      .map(v => s"Part ${v._2 + 1}: ${v._1}")
      .mkString("\n")
  )
}
