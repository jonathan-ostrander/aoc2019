package aoc

object Problem01 extends App {
  println(
    scala.io.Source.fromResource("inputs/1.txt")
      .getLines
      .takeWhile(_ != "")
      .map(_.toInt)
      .toList
      .map(m => List(
        (m / 3) - 2,
        Stream.continually(())
          .scanLeft(m)((t, _) => t / 3 - 2)
          .drop(1)
          .takeWhile(_ > 0)
          .sum
      ))
      .transpose
      .map(_.sum)
      .zipWithIndex
      .map(t => s"Part ${t._2 + 1}: ${t._1}")
      .mkString("\n")
  )
}
