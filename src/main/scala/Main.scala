
object Main {

  def data(file: String): List[String] =
    scala.io.Source.fromFile(file).getLines.toList

  def expense(xs: List[Int]): Int =
    xs
      .combinations(2)
      .collect { case xs @ a :: b :: Nil if (a + b == 2020) => xs }
      .flatten
      .foldLeft(1)(_ * _)

  def main(args: Array[String]): Unit = {
    val input = data("data/1a.txt")
    val xs = input.map(_.toInt)

    val result1 = expense(xs)
    println(result1)
  }

}
