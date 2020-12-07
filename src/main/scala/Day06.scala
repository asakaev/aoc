import scala.annotation.tailrec
import util.file

object Day06:

  // TODO: shared
  def group[A](xs: List[A])(p: A => Boolean): List[List[A]] =
    @tailrec def go(xs: List[A], s: (List[List[A]], List[A])): (List[List[A]], List[A]) =
      (xs, s) match {
        case (Nil, (acc, as)) => (as.reverse :: acc, Nil)
        case (h :: tl, (acc, as)) =>
          if p(h) then go(tl, (as.reverse :: acc, Nil))
          else go(tl, (acc, h :: as))
      }
    go(xs, (List.empty[List[A]], List.empty[A]))._1.reverse

  def chunks(xs: List[String]): List[List[String]] =
    group(xs)(_.isEmpty)

  // TODO: smarter?
  def solveA(xs: List[String]) =
    chunks(xs).map(_.mkString.toSet.size).sum

  // TODO: smarter?
  def solveB(xs: List[String]): Int =
    chunks(xs).map(_.map(_.toSet).reduce(_ & _).size).sum

  def main(args: Array[String]): Unit =
    val i = file.readAll("data/6a.txt")
    val r1 = solveA(i)
    val r2 = solveB(i)
    println(r1) // 7110
    println(r2) // 3628
