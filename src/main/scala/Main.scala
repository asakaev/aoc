import core.Semigroup
import core.{ given Semigroup[Int] }

object Main:

  def data(file: String): List[String] =
    scala.io.Source.fromFile(file).getLines.toList

  def part1(xs: List[Int]): Option[Int] =
    xs
      .combinations(2)
      .collectFirst { case xs as a :: b :: Nil if (a + b == 2020) => xs.reduce(_ combine _) }

  def part2(xs: List[Int]): Option[Int] =
    xs
      .combinations(3)
      .collectFirst {
        case xs as a :: b :: c :: Nil if (a + b + c == 2020) => xs.reduce(_ combine _)
      }

  def main(args: Array[String]): Unit =
    val i1 = data("data/1a.txt").map(_.toInt)
    val i2 = data("data/1b.txt").map(_.toInt)
    val r1 = part1(i1)
    val r2 = part2(i2)
    println(r1) // 1020099
    println(r2) // 49214880
