import fp.Semigroup
import fp.{ given Semigroup[Int] }
import util.file

object Day01:

  def solve(xs: List[Int], n: Int): Option[Int] =
    xs
      .combinations(n)
      .collectFirst { case xs if xs.sum == 2020 => xs.reduce(_ combine _) }

  def solveA(xs: List[Int]): Option[Int] =
    solve(xs, 2)

  def solveB(xs: List[Int]): Option[Int] =
    solve(xs, 3)

  def main(args: Array[String]): Unit =
    val i1 = file.readAll("data/1a.txt").map(_.toInt)
    val i2 = file.readAll("data/1b.txt").map(_.toInt)
    val r1 = solveA(i1)
    val r2 = solveB(i2)
    println(r1) // 1020099
    println(r2) // 49214880
